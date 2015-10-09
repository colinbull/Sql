#I "packages/FParsec/lib/net40-client/"
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
        
        type RefEx =
            | Ref of string list
           
        type ValueEx =
            | String of string
            | Float of float
            | Integer of int
            | Reference of RefEx
            | Ident of string
        
        type TermEx =
            | BinEx of (BinOp * TermEx * TermEx)
            | And of TermEx * TermEx
            | Or of TermEx * TermEx
            | Not of TermEx
            | UnEx of UnOp * TermEx
            | Value of ValueEx
        
        type OrderEx =
            | Order of string * OrderDir option
        
        type JoinEx =
            | Join of (JoinDir option * JoinType) * (RefEx * string option) * TermEx

        type ProjectionEx =
            | Projection of TermEx * string option

        type FromEx =
            | From of (RefEx * string option)
         
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
        let identifierString = many1Satisfy (fun c -> not(System.Char.IsWhiteSpace c) && isNoneOf ['[';']'] c)

        let keywords = [
            "SELECT"; "FROM"; "WHERE"; "JOIN"; "AS"; "GROUP"; "ORDER"; "HAVING"
            "BY"; "INNER"; "OUTER"; "LEFT"; "RIGHT"; "FULL"; "CROSS"; "ON"; "ASC"; "DESC";
            "AND"; "OR"; "NOT"; "LIKE"; "ORDER BY"; "DISTINCT"; "TOP"
        ]
        
        let str_ws s = pstring s .>> spaces
        let str_ws1 s = pstring s .>> spaces1

        let keywordSet = new HashSet<string>(keywords)
        let isKeyword (kw:string) = keywordSet.Contains(kw.ToUpper())
        
        let keyword (kw:string) : Parser<string, unit> = 
            pstringCI kw .>> spaces
            
        let identifier : Parser<string, unit> =
            let expectedIdentifier = expected "identifier"
            fun stream ->
                let state = stream.State
                let reply = identifierString stream
                if reply.Status = Ok && not (isKeyword reply.Result) 
                then reply
                else // result is keyword, so backtrack to before the string
                    stream.BacktrackTo(state)
                    Reply(Error, expectedIdentifier)
        

        let quotedStr = quote >>. manyCharsTill anyChar quote

        let strLiteral = quotedStr |>> Ast.String
        
        let numberLiteral =
            (pint32 |>> Ast.Integer) <|> (pfloat |>> Ast.Float)

        let alias =
            spaces >>. (
              (keyword "AS" >>. spaces >>. (quotedStr <|> identifier <|> (between (str_ws "[") (str_ws "]") identifier)))
              <|>
              (quotedStr <|> identifier) 
            ) .>> spaces
        
        let reference =
            sepBy (identifier <|> (between (str_ws "[") (str_ws "]") identifier)) (pchar '.')
            |>> Ref
        
        type Assoc = Associativity

        let valueEx =
            choice [
                strLiteral
                numberLiteral
                reference |>> Reference
            ] |>> Value

        let termEx =
            let opp = new OperatorPrecedenceParser<Ast.TermEx, unit, unit>()
            let expr = opp.ExpressionParser
            let term =
                spaces >>. (between (str_ws "(") (str_ws ")") expr <|> valueEx .>> spaces)
                
            opp.TermParser <- term

            opp.AddOperator(PrefixOperator("-", spaces, 1, true, (fun x -> UnEx(UnOp.Neg, x))))
            opp.AddOperator(PrefixOperator("NOT", notFollowedBy letter >>. spaces, 1, true, (fun x -> Not(x))))
            opp.AddOperator(PrefixOperator("LIKE", notFollowedBy letter >>. spaces, 1, true, (fun x -> UnEx(UnOp.Like, x))))

            opp.AddOperator(InfixOperator("*", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Mul, x, y))))
            opp.AddOperator(InfixOperator("/", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Div, x, y))))
            opp.AddOperator(InfixOperator("+", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Add, x, y))))
            opp.AddOperator(InfixOperator("-", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Sub, x, y))))
            opp.AddOperator(InfixOperator("%", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Mod, x, y))))
           
            opp.AddOperator(InfixOperator("=", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Eq, x, y))))
            opp.AddOperator(InfixOperator("<", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Lt, x, y))))
            opp.AddOperator(InfixOperator(">", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Gt, x, y))))
            opp.AddOperator(InfixOperator("<=", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Lte, x, y))))
            opp.AddOperator(InfixOperator(">=", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Gte, x, y))))

            opp.AddOperator(InfixOperator("AND", notFollowedBy letter >>. spaces, 1, Assoc.Left, (fun x y -> And(x,y))))
            opp.AddOperator(InfixOperator("OR", notFollowedBy letter >>. spaces, 1, Assoc.Left, (fun x y -> Or(x,y))))

            expr

        let referenceEx = 
            reference .>>. attempt(opt alias)

        let whereEx =
            opt <| attempt (spaces .>> (keyword "WHERE") .>> spaces >>. termEx)

        let orderEx =
            let direction = 
                opt (
                      ((keyword "ASC") >>% OrderDir.ASC)
                       <|>
                      ((keyword "DESC") >>% OrderDir.DESC)
                )
            
            let ids = 
                (spaces >>. (identifier .>>. attempt (spaces >>. direction .>> spaces)))

            attempt (spaces .>> keyword "ORDER BY" >>. (sepBy ids (pstring ",")) .>> spaces)
            |>> List.map OrderEx.Order

        let joinEx =
            let joinDir =
                spaces
                >>. (attempt (opt
                        (choice [
                                    keyword "LEFT" >>% JoinDir.Left
                                    keyword "RIGHT" >>% JoinDir.Right
                                ])))
                .>> spaces
            
            let joinType =
                spaces >>. choice [
                        keyword "JOIN" >>% JoinType.Inner
                        keyword "INNER JOIN" >>% JoinType.Inner
                        keyword "OUTER JOIN" >>% JoinType.Outer
                        keyword "FULL JOIN" >>% JoinType.Full
                        keyword "CROSS JOIN" >>% JoinType.Cross
                ] .>> spaces

            let joinClass =
                spaces >>. (joinDir .>>. joinType) .>> spaces
            
            let join =
                (joinClass .>>. referenceEx .>> spaces .>> keyword "ON" .>> spaces .>>. termEx .>> spaces)
                |>> (fun ((x,y),z) -> Join(x,y,z))
        
            attempt (manyTill join (notFollowedBy joinClass)) 
        
        let selectEx = 
            let projection = 
                termEx .>> spaces .>>. (opt (attempt alias))

            spaces >>. 
            keyword "SELECT" >>. opt (skipStringCI "DISTINCT" <|> skipStringCI "TOP") >>. 
            spaces >>. 
            sepBy (projection |>> Projection) (pstring ",") 

        let fromEx = 
            spaces >>. 
            keyword "FROM" >>. 
            spaces >>.
            (sepBy ((reference .>>. (opt (attempt alias))) |>> From) (pstring ","))

        let sqlParser =
            parse {
                do! spaces
                let! projection =  selectEx
                do! spaces
                let! from = fromEx
                do! spaces
                let! join = joinEx
                do! spaces 
                let! where = whereEx
                do! spaces
                let! order = orderEx
                do! spaces
                return { Projection = projection; 
                         From = from; 
                         Filters = where; 
                         Join = join; 
                         Order = order }
            }
                            
        let parse (str:string) =
            match run sqlParser (str.Trim()) with
            | Success(r,_,_) -> r
            | Failure(msg, err,_) -> failwithf "Failed to parse %s'" msg
            
open FParsec

let testSelect = 
    FParsec.CharParsers.run Sql.Parser.selectEx "SELECT (3 + 2) * 6 as 'Value', tbl1.ID ID"

let testWhere = 
    FParsec.CharParsers.run Sql.Parser.whereEx "WHERE ((Value = 36) AND (ID > 2)) OR (x + 1 > 3)"

let testOrderby = 
    FParsec.CharParsers.run Sql.Parser.orderEx "ORDER BY Value ASC, ID DESC"

let both = 
    let p = 
        parse { 
            let! where = Sql.Parser.whereEx
            do! spaces
            let! order = Sql.Parser.orderEx
            return (where, order)
        }
    FParsec.CharParsers.run p """WHERE ((Value = 36) AND (ID > 2)) OR ((x + 1) > 3) ORDER BY Value ASC, ID DESC"""

let test = """
SELECT (3 + 2) * 6 as 'Value', tbl1.ID ID
FROM dbo.Table1 tbl1
JOIN dbo.Table2 tbl2 ON Value = f.Value
RIGHT OUTER JOIN [dbo].[Table3] tbl3 on tbl2.Value = tbl3.NotionalAmount
LEFT OUTER JOIN dbo.Table4 tbl4 on tbl1.Value = tbl4.NotionalAmount
WHERE ((Value = 36) AND (ID > 2)) OR ((x + 1) > 3)
ORDER BY Value ASC
"""

Sql.Parser.parse test

FParsec.CharParsers.runParserOnFile Sql.Parser.sqlParser () @"C:\Appdev\sql\examples\ef_default_query.sql" System.Text.Encoding.UTF8
