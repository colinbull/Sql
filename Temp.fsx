
        
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

            let fromEx =
                let nestedQuery =
                    between (str_ws "(") (str_ws ")") sqlParser |>> QueryEx
                    .>> spaces
                    .>>. (opt (attempt alias)) |>> From

                let internalFrom =
                    nestedQuery |>> List.singleton
                    <|>
                    (sepBy (reference .>>. (opt (attempt alias)) |>> From) (pstring ","))
                
                spaces >>. 
                keyword "FROM" >>. 
                spaces >>.
                internalFrom   

            
            spaces >>. 
            keyword "SELECT" >>. opt (skipStringCI "DISTINCT") >>. 
            spaces >>. 
            sepBy (projection |>> Projection) (pstring ",") .>>
            spaces .>>.
            fromEx
                    
        do sqlParserRef :=
            parse {
                do! spaces
                let! (projection, from) =  selectEx
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


let testSelect = 
    FParsec.CharParsers.run Sql.Parser.selectEx "SELECT (3 + 2) * 6 as 'Value', tbl1.ID ID FROM dbo.Table1 tbl1"


let testSelectNested = 
    FParsec.CharParsers.run Sql.Parser.selectEx """
        SELECT (3 + 2) * 6 as 'Value', tbl1.ID ID
        FROM (SELECT quux FROM dbo.Table3)
    """


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
    FParsec.CharParsers.run p """
    WHERE ((Value = 36) AND (ID > 2)) OR ((x + 1) > 3)
    ORDER BY Value ASC, ID DESC"""

let test = """
SELECT (3 + 2) * 6 as 'Value', tbl1.ID ID
FROM (SELECT quux FROM dbo.Table3)
JOIN dbo.Table2 tbl2 ON Value = f.Value AND tbl2.Key = N'MyKey'
RIGHT OUTER JOIN [dbo].[Table3] tbl3 on tbl2.Value = tbl3.NotionalAmount
LEFT OUTER JOIN dbo.Table4 tbl4 on tbl1.Value = tbl4.NotionalAmount
WHERE ((Value = 36) AND (ID > 2)) OR ((x + 1) > 3)
ORDER BY Value ASC"""

