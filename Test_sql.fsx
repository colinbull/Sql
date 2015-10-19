#load "SqlAnalyzer.fsx"

module ParserTests =
    open SqlAnalyzer

    let runParser p s =
        FParsec.CharParsers.run p s

    let ``null``() =
        runParser Sql.Parser.primitiveEx "null"

    let ``string literal is null``() =
        runParser Sql.Parser.termEx "2 IS NULL"
    
    let ``n prefixed string``() =
        runParser Sql.Parser.quotedStr "N'Foo'"

    let ``empty n prefixed string``() =
        runParser Sql.Parser.quotedStr "N''"
      
    let ``references as identifiers``() =
        runParser Sql.Parser.reference "t"

    let ``multipart references``() =
        runParser Sql.Parser.reference "t.A"

    let ``star as a reference``() =
        runParser Sql.Parser.reference "*"

    let ``term as arithemtic``() =
        runParser Sql.Parser.termEx "1 + 2"

    let ``term as reference``() =
        runParser Sql.Parser.termEx "t.A"

    let ``term as boolean ex``() =
        runParser Sql.Parser.termEx "true AND false"

    let ``term with alias``() =
        runParser Sql.Parser.aliasedTermEx "t.A A"

    let ``term without alias``() =
        runParser Sql.Parser.aliasedTermEx "t.A"

    let ``term as star``() =
        runParser Sql.Parser.aliasedTermEx "*"

    let ``case with one branch``() =
        runParser Sql.Parser.caseEx "case when foo = 2 then 2 else 4 end"

    let ``case with multiple branch``() =
        runParser Sql.Parser.caseEx "case foo when 2 then 'Two' when 4 THEN 'Four' ELSE 'Other' END"  

    let ``case with calls``() =
        runParser Sql.Parser.caseEx "CASE WHEN (([Extent4].[alloc_num] IS NULL) AND ([Extent4].[alloc_item_num] IS NULL)) THEN [Extent2].[del_date_from] ELSE  CAST([Extent4].[nomin_date_from] AS datetime) END"
    
    let ``call``()  =
        runParser Sql.Parser.callEx "LTRIM(f.F)"

    let ``call with leading space``()  =
        runParser Sql.Parser.callEx "LTRIM( f.F)"

    let ``call multiple parameters``() =
        runParser Sql.Parser.callEx "UPPER(f.b,b)"

    let ``call with expr``() =
        Sql.Parser.parse "CAST(NULL AS int)"

    let ``where as equality``() =
        runParser Sql.Parser.whereEx "where f.F = 2"

    let ``join as equality``() =
        runParser Sql.Parser.joinEx "join bar b ON (b.B = f.F)"

    let ``join with brackets``() =
        runParser Sql.Parser.joinEx "join (bar) b ON b.B = f.F"

    let ``select from``() =
        Sql.Parser.parse "SELECT * FROM foo"

    let ``select with alias``() =
        Sql.Parser.parse "SELECT * FROM foo f"

    let ``select with alias using as``() =
        Sql.Parser.parse "SELECT * FROM foo as f"
        
    let ``select with projection``() =
        Sql.Parser.parse "SELECT t FROM foo f"

    let ``select with multipart reference projection``() =
        Sql.Parser.parse "SELECT t.A FROM foo f"

    let ``select in brackets``() =
        Sql.Parser.parse "SELECT * FROM (foo f)"
   
    let ``select with expression``() =
        Sql.Parser.parse "SELECT 1 * 2 FROM foo"

    let ``select with complex expression``() =
        Sql.Parser.parse "SELECT ((1 * (2 + f.F) % 2) = 0) FROM foo f"

    let ``select from where``() =
        Sql.Parser.parse "SELECT * FROM foo WHERE f.Value = 2"

    let ``select from join``() =
        Sql.Parser.parse """SELECT * FROM foo f INNER JOIN bar as b ON (b.B = f.F)"""

    let ``select from join where``() =
        Sql.Parser.parse """
            SELECT * FROM foo f
            JOIN bar b ON b.B = f.F
            WHERE f.F = 2
        """

    let ``select from join with brackets``() =
        Sql.Parser.parse """
            SELECT * FROM foo f
           	INNER JOIN [dbo].[trade_item_wet_phy] AS [Extent2] ON
                        ([Extent1].[trade_num] = [Extent2].[trade_num])
                        AND ([Extent1].[order_num] = [Extent2].[order_num])
                        AND ([Extent1].[item_num] = [Extent2].[item_num])
            WHERE f.F = 2
           """
    
    let ``select with nested join``() =
        Sql.Parser.parse """
        SELECT * FROM foo f
        INNER JOIN (SELECT *    
                     FROM bar AS b)
        AS c ON (c.A = f.A)
        """
    
    let ``select distinct from``() =
        Sql.Parser.parse """SELECT DISTINCT * FROM foo f """

    let ``select top from``() =
        Sql.Parser.parse """SELECT TOP 100 * FROM foo f"""

    let ``select from groupby``() =
        Sql.Parser.parse "SELECT * FROM foo f GROUP BY f.Value"
                    
    let ``select with nested query``() =
        Sql.Parser.parse "SELECT * FROM (SELECT * FROM foo f)"

    let ``select with nested query aliased``() =
        Sql.Parser.parse "SELECT * FROM (SELECT * FROM foo f) t"

    let ``example file``() =
        Sql.Parser.parse (System.IO.File.ReadAllText("examples/subquery.sql"))
