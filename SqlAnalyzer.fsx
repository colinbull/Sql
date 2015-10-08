#I "../packages/FParsec.1.0.1/lib/net40-client/"
#r "FParsecCS.dll"
#r "FParsec.dll"

module Sql =


    module Ast =

        type BinOp =
            | Gt | Lt | Gte | Lte | Eq
            | Add | Mul | Div | Sub | Mod

        type UnOp =
            | Like | Neg           

        type JoinDir =
            | Left | Right
        
        type JoinType =
            | Inner | Outer | Full | Cross
        
        type OrderDir =
            | ASC | DESC
        
        type Alias = string

        type RefEx =
            | Ref of string list
           
        type ValueEx =
            | String of string
            | Float of float
            | Integer of int
            | Reference of RefEx
        
        type TermEx =
            | BinEx of (BinOp * TermEx * TermEx)
            | And of TermEx * TermEx
            | Or of TermEx * TermEx
            | Not of TermEx
            | UnEx of UnOp * TermEx
            | Value of ValueEx
        
        type OrderEx =
            | Order of RefEx * OrderDir option
        
        type JoinEx =
            | Join of (JoinDir option * JoinType) * (RefEx * Alias option) * TermEx

        type ProjectionEx =
            | Projection of TermEx * Alias option

        type FromEx =
            | From of (RefEx * Alias option)
         
        type Query = {
            Projection : ProjectionEx list
            Filters : TermEx option
            Order : OrderEx list
            Join : JoinEx list
            From : FromEx list
        }
         
    module Parser =

        open FParsec
        open FParsec.Primitives
        open FParsec.CharParsers
        open System.Collections.Generic
        open Ast

        let quote = skipStringCI "\"" <|> skipStringCI "'"
        let identifierString = many1Satisfy (fun c -> isAsciiLetter c || isDigit c) .>> spaces

        let keywords = [
            "SELECT"; "FROM"; "WHERE"; "JOIN"; "AS"; "GROUP"; "ORDER"; "HAVING"
            "BY"; "INNER"; "OUTER"; "LEFT"; "RIGHT"; "FULL"; "CROSS"; "ON"; "ASC"; "DESC"
        ]
        
        let keywordSet = new HashSet<string>(keywords)
        let isKeyword (kw:string) = keywordSet.Contains(kw.ToUpper())
        
        let identifier : Parser<string, unit> =
            let expectedIdentifier = expected "identifier"
            fun stream ->
                let state = stream.State
                let reply = identifierString stream
                if reply.Status <> Ok || not (isKeyword reply.Result) then reply
                else // result is keyword, so backtrack to before the string
                    stream.BacktrackTo(state)
                    Reply(Error, expectedIdentifier)

        let quotedStr = quote >>. manyCharsTill anyChar quote

        let strLiteral = quotedStr |>> Ast.String
        
        let numberLiteral =
            (pint32 |>> Ast.Integer) <|> (pfloat |>> Ast.Float)

        let alias =
            attempt (spaces >>. (identifier <|> (skipStringCI "AS" >>. spaces >>. quotedStr)))
        
        let reference =
            sepBy identifier (pchar '.')
            |>> Ref
        
        type Assoc = Associativity

        let valueEx =
            spaces >>. choice [
                strLiteral
                numberLiteral
                reference |>> Reference
            ] |>> Value
            .>> spaces
        
        let termEx =
            let opp = new OperatorPrecedenceParser<Ast.TermEx, unit, unit>()
            let expr = opp.ExpressionParser
            let term =
                spaces >>.
                (between (pstring "(") (pstring ")") expr
                 <|>
                 valueEx)
                .>> spaces
            opp.TermParser <- term

            opp.AddOperator(PrefixOperator("-", spaces, 1, false, (fun x -> UnEx(UnOp.Neg, x))))
            opp.AddOperator(InfixOperator("*", spaces, 2, Assoc.Left, (fun x y -> BinEx(BinOp.Mul, x, y))))
            opp.AddOperator(InfixOperator("/", spaces, 2, Assoc.Left, (fun x y -> BinEx(BinOp.Div, x, y))))
            opp.AddOperator(InfixOperator("+", spaces, 3, Assoc.Left, (fun x y -> BinEx(BinOp.Add, x, y))))
            opp.AddOperator(InfixOperator("-", spaces, 3, Assoc.Left, (fun x y -> BinEx(BinOp.Sub, x, y))))
            opp.AddOperator(InfixOperator("%", spaces, 3, Assoc.Left, (fun x y -> BinEx(BinOp.Mod, x, y))))
           
            opp.AddOperator(InfixOperator("=", spaces, 4, Assoc.Left, (fun x y -> BinEx(BinOp.Eq, x, y))))
            opp.AddOperator(InfixOperator("<", spaces, 4, Assoc.Left, (fun x y -> BinEx(BinOp.Lt, x, y))))
            opp.AddOperator(InfixOperator(">", spaces, 4, Assoc.Left, (fun x y -> BinEx(BinOp.Gt, x, y))))
            opp.AddOperator(InfixOperator("<=", spaces, 4, Assoc.Left, (fun x y -> BinEx(BinOp.Lte, x, y))))
            opp.AddOperator(InfixOperator(">=", spaces, 4, Assoc.Left, (fun x y -> BinEx(BinOp.Gte, x, y))))

            opp.AddOperator(InfixOperator("AND", spaces, 5, Assoc.None, (fun x y -> And(x,y))))
            opp.AddOperator(InfixOperator("OR", spaces, 6, Assoc.None, (fun x y -> Or(x,y))))
            opp.AddOperator(PrefixOperator("NOT", spaces, 7, false, (fun x -> Not(x))))
            opp.AddOperator(PrefixOperator("LIKE", spaces, 7, false, (fun x -> UnEx(UnOp.Like, x))))
            
            expr

        let whereEx =
            opt <| attempt (spaces .>> skipStringCI "WHERE" >>. spaces >>. termEx .>> spaces)

        let orderEx =
            attempt (skipStringCI "ORDER BY" >>. spaces
                     >>. sepBy (spaces >>. reference .>>. (
                                                attempt (
                                                      opt (
                                                            (skipStringCI  "ASC") >>% OrderDir.ASC
                                                             <|>
                                                            ((skipStringCI "DESC") >>% OrderDir.DESC)
                                                      )
                                                  )
                                               )
                                .>> spaces
                         ) (pstring ",")
                    ) |>> List.map Order

        let joinEx =
            let joinDir =
                spaces
                >>. (attempt (opt
                        (choice [
                                    pstringCI "LEFT" >>% JoinDir.Left
                                    pstringCI "RIGHT" >>% JoinDir.Right
                                ])))
                .>> spaces
            
            let joinType =
                spaces >>. choice [
                        pstringCI "JOIN" >>% JoinType.Inner
                        pstringCI "INNER JOIN" >>% JoinType.Inner
                        pstringCI "OUTER JOIN" >>% JoinType.Outer
                        pstringCI "FULL JOIN" >>% JoinType.Full
                        pstringCI "CROSS JOIN" >>% JoinType.Cross
                ] .>> spaces

            let joinClass =
                spaces >>. (joinDir .>>. joinType) .>> spaces
            
            let join =
                (joinClass .>>. ((reference .>>. (opt alias))  .>> spaces .>> skipStringCI "ON" .>> spaces .>>. termEx .>> spaces))
                |>> (fun (x,(y,z)) -> Join(x,y,z))
        
            attempt (manyTill join (notFollowedBy joinClass)) 
            
        
        let sqlParser =
            parse {
                do! skipStringCI "SELECT" >>. spaces
                let! projection =  sepBy (termEx .>>. (opt alias) |>> Projection) (pstring ",")
                do! spaces >>. skipStringCI "FROM" >>. spaces
                let! from = (sepBy ((reference .>>. (opt alias)) |>> From) (pstring ","))
                do! spaces
                let! join = joinEx
                do! spaces 
                let! where = whereEx
                do! spaces
                let! order = orderEx
                do! spaces
                return { Projection = projection; From = from; Filters = where; Join = join; Order = order }
            }
                            
        let parse (str:string) =
            match run sqlParser (str.Trim()) with
            | Success(r,_,_) -> r
            | Failure(msg, err,_) -> failwithf "Failed to parse %s'" msg
            

let test = """
SELECT (3 + 2) * 6 as 'Value', tbl1.ID ID
FROM dbo.Table1 tbl1
JOIN dbo.Table2 tbl2 on Value = f.Value
RIGHT OUTER JOIN dbo.Table3 tbl3 on tbl2.Value = tbl3.NotionalAmount
LEFT OUTER JOIN dbo.Table4 tbl4 on tbl1.Value = tbl4.NotionalAmount
WHERE (Value = 36) AND (ID > 2)
ORDER BY Value ASC, ID
"""

Sql.Parser.parse test
