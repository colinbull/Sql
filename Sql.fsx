#I "packages/FParsec/lib/net40-client/"
#r "FParsecCS.dll"
#r "FParsec.dll"

module Sql =
    
    open FParsec
    open FParsec.Primitives
    open FParsec.CharParsers
    open System.Collections.Generic

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

        type ValueEx =
            | String of string
            | Float of float
            | Integer of int
            | Bool of bool
            | Null
            
        type OrderEx =
            | Order of string * OrderDir option
        
        type TermEx =
            | BinEx of (BinOp * TermEx * TermEx)
            | And of TermEx * TermEx
            | Or of TermEx * TermEx
            | Not of TermEx
            | UnEx of UnOp * TermEx
            | Value of ValueEx
            | Ref of string list
            | Cast of TermEx * string
            | Call of string * TermEx list
            | Case of TermEx option * (TermEx * TermEx) list * TermEx
            | QueryEx of Query

            
        and ProjectionEx =
            | Projection of TermEx * string option
            | Distinct of ProjectionEx list
            | Top of int * ProjectionEx list

        and JoinEx =
            | Join of (JoinDir option * JoinType) * (TermEx * string option) * TermEx
        
        and FromEx =
            | From of (TermEx * string option) 

        and GroupByEx =
            | Group of TermEx * string option
                    
        and Query = {
            Projection : ProjectionEx list
            Filters : TermEx option
            Order : OrderEx list option
            Join : JoinEx list
            From : FromEx list
            GroupBy : GroupByEx list option
        }

        type Assoc = Associativity
        
    module Parser =

        open System
        open Ast

        let symbols = "[]\"'()*,.".ToCharArray()
        let quote = skipStringCI "\"" <|> skipStringCI "'"
        let identifierString = many1Satisfy (fun c -> not(System.Char.IsWhiteSpace c) && isNoneOf symbols c)

        let keywords = [
            "SELECT"; "FROM"; "WHERE"; "JOIN"; "AS"; "GROUP"; "ORDER"; "HAVING"
            "BY"; "INNER"; "OUTER"; "LEFT"; "RIGHT"; "FULL"; "CROSS"; "ON"; "ASC"; "DESC";
            "AND"; "OR"; "NOT"; "LIKE"; "ORDER BY"; "DISTINCT"; "TOP"; "CASE"; "WHEN"; "THEN";
            "END"; "IS"; "NULL"; "TRUE"; "FALSE";
        ]
        
        let str_ws s = pstring s .>> spaces
        let str_ws1 s = pstring s .>> spaces1
        let between_str a b p = between (str_ws a) (str_ws b) p 
        
        let keywordSet = new HashSet<string>(keywords)
        let isKeyword (kw:string) = keywordSet.Contains(kw.ToUpper())
        
        let keyword (kw:string) = 
            spaces >>. skipStringCI kw >>. spaces
            
        let identifier : Parser<string, unit> =
            let expectedIdentifier = expected "identifier"
            fun stream ->
                let state = stream.State
                let reply = identifierString stream
                if reply.Status = Ok && not(isKeyword reply.Result) 
                then reply
                else // result is keyword, so backtrack to before the string
                    stream.BacktrackTo(state)
                    Reply(Error, expectedIdentifier)
        
        let quotedStr = ((skipChar 'N' >>.  quote) <|> quote) >>. manyCharsTill anyChar quote

        let strLiteral = quotedStr |>> Ast.String
        
        let numberLiteral =
            (pint32 |>> Ast.Integer) <|> (pfloat |>> Ast.Float)

        let nullLiteral = keyword "NULL" >>% Null

        let boolLiteral =
            choice [
                keyword "TRUE" |>> (fun _ -> Bool true)
                keyword "FALSE" |>> (fun _ -> Bool false)
            ]
                
        let primitiveLiteral =
            choice [
                nullLiteral
                boolLiteral
                strLiteral
                numberLiteral
            ]

        let primitiveEx =
            spaces >>. primitiveLiteral .>> spaces
            |>> Value
            
        let alias =
            spaces >>. (
              (keyword "AS" >>. (quotedStr <|> identifier <|> (between_str "["  "]" identifier)))
              <|>
              (quotedStr <|> identifier) 
            ) .>> spaces
        
        let reference =
            let r = 
                spaces >>.
                sepBy1 ((pstring "*")
                        <|> identifier
                        <|> (between_str "[" "]" identifier)
                       ) (pchar '.')
                .>> spaces
                |>> Ref
            between_str "(" ")" r
            <|>
            r
                
        let (sqlParser, sqlParserRef) = createParserForwardedToRef()
                
        let termEx =
            let opp = new OperatorPrecedenceParser<Ast.TermEx, unit, unit>()
            let expr = opp.ExpressionParser
            let term =
                spaces >>. ((between_str "("  ")" expr) <|> (between_str "("  ")" sqlParser) <|> sqlParser) .>> spaces
                
            opp.TermParser <- term

            opp.AddOperator(PrefixOperator("-", spaces, 1, true, (fun x -> UnEx(UnOp.Neg, x))))
            opp.AddOperator(PrefixOperator("NOT", notFollowedBy letter >>. spaces, 1, true, (fun x -> Not(x))))
            opp.AddOperator(PrefixOperator("LIKE", notFollowedBy letter >>. spaces, 1, true, (fun x -> UnEx(UnOp.Like, x))))

            opp.AddOperator(InfixOperator("*", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Mul, x, y))))
            opp.AddOperator(InfixOperator("/", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Div, x, y))))
            opp.AddOperator(InfixOperator("+", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Add, x, y))))
            opp.AddOperator(InfixOperator("-", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Sub, x, y))))
            opp.AddOperator(InfixOperator("%", spaces, 1, Assoc.Left, (fun x y -> BinEx(BinOp.Mod, x, y))))
        
            opp.AddOperator(InfixOperator("IS", notFollowedBy letter >>. spaces, 2, Assoc.None, (fun x y -> BinEx(BinOp.Eq, x, y))))  
            opp.AddOperator(InfixOperator("=", spaces, 1, Assoc.None, (fun x y -> BinEx(BinOp.Eq, x, y))))
            opp.AddOperator(InfixOperator("<", spaces, 1, Assoc.None, (fun x y -> BinEx(BinOp.Lt, x, y))))
            opp.AddOperator(InfixOperator(">", spaces, 1, Assoc.None, (fun x y -> BinEx(BinOp.Gt, x, y))))
            opp.AddOperator(InfixOperator("<=", spaces, 1, Assoc.None, (fun x y -> BinEx(BinOp.Lte, x, y))))
            opp.AddOperator(InfixOperator(">=", spaces, 1, Assoc.None, (fun x y -> BinEx(BinOp.Gte, x, y))))

            opp.AddOperator(InfixOperator("AND", notFollowedBy letter >>. spaces, 1, Assoc.Left, (fun x y -> And(x,y))))
            opp.AddOperator(InfixOperator("OR", notFollowedBy letter >>. spaces, 1, Assoc.Left, (fun x y -> Or(x,y))))

            between_str "(" ")" expr
            <|>
            expr

        
        let aliasedTermEx =
            let t = (termEx .>>. (opt (attempt alias)))
            spaces >>. t .>> spaces
            
        let termOrCastEx =
            let cast =
                attempt (opt (keyword "AS" >>.  identifier))
            spaces >>. termEx .>> spaces .>>. cast .>> spaces
            |>> function
                | a, None -> a
                | a, Some b -> Cast(a, b)

        let caseEx =
            let cases =
                manyTill (keyword "WHEN" >>. (termOrCastEx <|> termEx)
                          .>> keyword "THEN"
                           .>>. (termOrCastEx <|> termEx)
                          ) (keyword "ELSE") 
                .>>.  (termOrCastEx <|> termEx) .>> keyword "END"
               
            keyword "CASE" >>. (attempt (opt termEx)) .>>. cases
            |>> (fun (cond, (cases, terminal)) -> Case(cond, cases, terminal))

        let callEx =
            identifier .>>. between_str "(" ")" (sepBy (termOrCastEx <|> termEx) (pstring ","))
            |>> Call
        
        let selectEx =
            let projections =
                sepBy1 (aliasedTermEx |>> Projection) (pstring ",")    
            
            let modifiers =
                choice [
                    keyword "DISTINCT" >>. projections |>> fun x -> [Distinct x]
                    keyword "TOP" >>. pint32 .>> spaces .>>. projections |>> (fun (i, ps) ->  [Top(i, ps)])
                    projections
                ]
            
            keyword "SELECT" >>. modifiers
           
        let fromEx =
            keyword "FROM" >>.
            sepBy1 (aliasedTermEx |>> From) (pstring ",")

        let whereEx = keyword "WHERE" >>. termEx

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
                (joinDir .>>. joinType)
            
            let join =
                (joinClass .>>. aliasedTermEx .>> keyword "ON" .>>. termEx)
                |>> (fun ((x,y),z) -> Join(x,y,z))
        
            manyTill join (notFollowedBy joinClass)

        let groupByEx =
            keyword "GROUP BY" >>. sepBy1 (aliasedTermEx |>> Group) (pstring ",")    
        
        let orderEx =
            let direction = 
                opt (
                      ((keyword "ASC") >>% OrderDir.ASC)
                       <|>
                      ((keyword "DESC") >>% OrderDir.DESC)
                )
            
            let ids = 
                (spaces >>. (identifier .>>. attempt (spaces >>. direction .>> spaces)))

            keyword "ORDER BY" >>. (sepBy ids (pstring ","))
            |>> List.map OrderEx.Order
        
        let queryEx =
            parse {
                let! select = selectEx
                let! from = fromEx
                let! join = attempt joinEx
                let! where = attempt (opt whereEx)
                let! group = attempt (opt groupByEx)
                let! order = attempt (opt orderEx)
                return {
                   Projection = select
                   Filters = where
                   Order = order
                   Join = join
                   From = from
                   GroupBy = group
                }
            } |>> QueryEx
            
        do
           sqlParserRef :=
                choice [
                    attempt callEx
                    primitiveEx
                    reference
                    caseEx
                    queryEx
                ]
        
        let parse (str:string) =
            match run sqlParser (str.Trim()) with
            | Success(r,_,_) -> r
            | Failure(msg, err,_) -> failwithf "Failed to parse %s'" msg
