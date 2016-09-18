#load "TestRunner.fs"
#load "Sql.fsx"

[<Test.Runner.TestFixture>]
module SqlParserTests =

    open Sql
    open Sql.Sql.Ast    
    open FParsec
    
    let runParser p s =
        match FParsec.CharParsers.run p s with
        | Success(x,_,_) -> x
        | Failure(msg, err, _) -> failwithf "Failed %A" msg

    let ``null``() =
        runParser Sql.Parser.primitiveEx "null" = Value(Null)
        
    let ``string literal is null``() =
        runParser Sql.Parser.termEx "2 IS NULL" = (BinEx(BinOp.Eq, Value(Integer 2), Value(Null)))
    
    let ``n prefixed string``() =
        runParser Sql.Parser.quotedStr "N'Foo'" = "Foo"

    let ``empty n prefixed string``() =
        runParser Sql.Parser.quotedStr "N''" = ""
      
    let ``references as identifiers``() =
        runParser Sql.Parser.reference "t" = Ref(["t"])

    let ``multipart references``() =
        runParser Sql.Parser.reference "t.A" = Ref(["t"; "A"])

    let ``star as a reference``() =
        runParser Sql.Parser.reference "*" = Ref(["*"])

    let ``term as arithemtic``() =
        runParser Sql.Parser.termEx "1 + 2" = BinEx(BinOp.Add, Value(Integer 1), Value(Integer 2))

    let ``term as reference``() =
        runParser Sql.Parser.termEx "t.A" = Ref(["t"; "A"])

    let ``term as boolean ex``() =
        runParser Sql.Parser.termEx "true AND false" = And(Value(Bool true), Value(Bool false))

    let ``term with alias``() =
        runParser Sql.Parser.aliasedTermEx "t.A A" = (Ref(["t"; "A"]), Some "A")

    let ``term without alias``() =
        runParser Sql.Parser.aliasedTermEx "t.A" = (Ref(["t"; "A"]), None)

    let ``term as star``() =
        runParser Sql.Parser.aliasedTermEx "*" = (Ref(["*"]), None)

    let ``case with one branch``() =
        let actual = runParser Sql.Parser.caseEx "case when foo = 2 then 2 else 4 end"
        let expected = Case(None, [BinEx(BinOp.Eq, Ref(["foo"]), Value(Integer 2)), Value(Integer 2)], Value(Integer 4))
        actual = expected
        
    let ``case with multiple branch``() =
        let actual = runParser Sql.Parser.caseEx "case foo when 2 then 'Two' when 4 THEN 'Four' ELSE 'Other' END"  
        let expected = Case(Some(Ref ["foo"]),[Value(Integer 2), Value(String "Two"); Value(Integer 4), Value(String "Four")], Value(String "Other"))
        actual = expected
     
    let ``case with calls``() =
        let actual = runParser Sql.Parser.caseEx "CASE WHEN ((a IS NULL) AND (b IS NULL)) THEN c ELSE  CAST(d AS datetime) END"
        let expected = Case(None,[And(BinEx(Eq, Ref["a"], Value(Null)), BinEx(Eq, Ref["b"], Value(Null))), Ref["c"]], Call("CAST", [Cast(Ref ["d"], "datetime")]))
        actual = expected
        
    let ``call``()  =
        runParser Sql.Parser.callEx "LTRIM(f.F)" = Call("LTRIM", [Ref ["f";"F"]])

    let ``call with leading space``()  =
        runParser Sql.Parser.callEx "LTRIM( f.F)" = Call("LTRIM", [Ref ["f";"F"]])

    let ``call multiple parameters``() =
        runParser Sql.Parser.callEx "UPPER(f.b,b)" = Call("UPPER", [Ref ["f";"b"]; Ref["b"]])

    let ``call with expr``() =
        Sql.Parser.parse "CAST(NULL AS int)" = Call("CAST", [Cast(Value(Null), "int")])

    let ``where as equality``() =
        runParser Sql.Parser.whereEx "where f.F = 2" = BinEx(Eq, Ref["f";"F"], Value(Integer 2))

    let ``join as equality``() =
        let actual = runParser Sql.Parser.joinEx "join bar b ON (b.B = f.F)"
        let expected = [Join((None, Inner), (Ref ["bar"], Some "b"), BinEx(Eq, Ref["b";"B"], Ref["f";"F"]))]
        actual = expected

    let ``join with brackets``() =
        let actual = runParser Sql.Parser.joinEx "join (bar) b ON b.B = f.F"
        let expected = [Join((None, Inner), (Ref ["bar"], Some "b"), BinEx(Eq, Ref["b";"B"], Ref["f";"F"]))]
        actual = expected

    let ``select from``() =
        let actual = Sql.Parser.parse "SELECT * FROM foo"
        let expected = { Projection = [Projection(Ref ["*"], None)];
                         From = [From(Ref ["foo"], None)];
                         Join = []
                         Filters = None; Order = None; GroupBy = None } |> QueryEx
        actual = expected

    let ``select with alias``() =
        let actual = Sql.Parser.parse "SELECT * FROM foo f"
        let expected = { Projection = [Projection(Ref ["*"], None)];
                         From = [From(Ref ["foo"], Some "f")];
                         Join = []
                         Filters = None; Order = None; GroupBy = None } |> QueryEx
        actual = expected


    let ``select with alias using as``() =
        let actual = Sql.Parser.parse "SELECT * FROM foo as f"
        let expected = { Projection = [Projection(Ref ["*"], None)];
                         From = [From(Ref ["foo"], Some "f")];
                         Join = []
                         Filters = None; Order = None; GroupBy = None } |> QueryEx
        actual = expected

    let ``select with projection``() =
        let actual = Sql.Parser.parse "SELECT t FROM foo f"
        let expected = { Projection = [Projection(Ref ["t"], None)];
                         From = [From(Ref ["foo"], Some "f")];
                         Join = []
                         Filters = None; Order = None; GroupBy = None } |> QueryEx
        actual = expected

    let ``select with multipart reference projection``() =
        let actual = Sql.Parser.parse "SELECT t.A FROM foo f"
        let expected = { Projection = [Projection(Ref ["t";"A"], None)];
                         From = [From(Ref ["foo"], Some "f")];
                         Join = []
                         Filters = None; Order = None; GroupBy = None } |> QueryEx
        actual = expected

        
    let ``select in brackets``() =
        let actual = Sql.Parser.parse "SELECT * FROM (foo f)"
        let expected = { Projection = [Projection(Ref ["*"], None)];
                         From = [From(Ref ["foo"], Some "f")];
                         Join = []
                         Filters = None; Order = None; GroupBy = None } |> QueryEx
        actual = expected
       
           
    let ``select with expression``() =
        let actual = Sql.Parser.parse "SELECT 1 * 2 FROM foo"
        let expected = { Projection = [Projection(BinEx(Mul, Value(Integer 1), Value(Integer 2)), None)];
                         From = [From(Ref ["foo"], None)];
                         Join = []
                         Filters = None; Order = None; GroupBy = None } |> QueryEx
        actual = expected
        
    let ``select with complex expression``() =
        let actual = Sql.Parser.parse "SELECT ((1 * (2 + f.F) % 2) = 0) FROM foo f"
        let expected = { Projection = [
                                       Projection(BinEx(Eq,
                                                    BinEx(Mod,
                                                      BinEx(Mul, Value(Integer 1), BinEx(Add, Value(Integer 2), Ref ["f";"F"])),
                                                           Value(Integer 2)), Value(Integer 0)), None)];
                        From = [From(Ref ["foo"], Some "f")];
                        Join = [];
                        Filters = None; Order = None; GroupBy = None } |> QueryEx
        actual = expected

    let ``select from where``() =
        let actual = Sql.Parser.parse "SELECT * FROM foo WHERE f.Value = 2"
        let expected = { Projection = [Projection(Ref ["*"], None)];
                         From = [From(Ref ["foo"], None)]
                         Join = []; Order = None; GroupBy = None
                         Filters = Some(BinEx(Eq, Ref ["f"; "Value"], Value(Integer 2))) } |> QueryEx
        actual = expected
                         

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

Test.Runner.run()