#load "SqlAnalyzer.fsx"

module ParserTests =
    open SqlAnalyzer

    let runParser p s =
        FParsec.CharParsers.run p s
    
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

    let ``where as equality``() =
        runParser Sql.Parser.whereEx "where f.F = 2"

    let ``join as equality``() =
        runParser Sql.Parser.joinEx "join bar b ON (b.B = f.F)"

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
        Sql.Parser.parse """SELECT * FROM foo f JOIN bar b ON (b.B = f.F)"""

    let ``select from join where``() =
        Sql.Parser.parse """
            SELECT * FROM foo f
            JOIN bar b ON b.B = f.F
            WHERE f.F = 2
        """
    let ``select distinct from``() =
        Sql.Parser.parse """SELECT DISTINCT * FROM foo f """

    let ``select top from``() =
        Sql.Parser.parse """SELECT TOP 100 * FROM foo f"""
                    
    let ``select with nested query``() =
        Sql.Parser.parse "SELECT * FROM (SELECT * FROM foo f)"

    let ``select with nested query aliased``() =
        Sql.Parser.parse "SELECT * FROM (SELECT * FROM foo f) t"
