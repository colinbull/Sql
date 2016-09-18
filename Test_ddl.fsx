#load "TestRunner.fs"
#load "Ddl.fsx"

module DdlParserTests =

    open Ddl
    open Ddl.Ddl.Ast    
    open FParsec

    let runParser p s =
        match FParsec.CharParsers.run p s with
        | Success(x,_,_) -> x
        | Failure(msg, err, _) -> failwithf "Failed %A" msg
    
    [<Test.Runner.TestFixture>]
    module Drop = 

        let ``drop without cascade or purge``() =
            let expected = Drop(Table(Ident [Name("PRICES")]), false, false)
            let actual = runParser Ddl.Parser.pdrop "DROP TABLE PRICES"
            actual = expected

        let ``drop without purge``() =
            let expected = Drop(Table(Ident [Name("PRICES")]), true, false)
            let actual = runParser Ddl.Parser.pdrop "DROP TABLE PRICES CASCADE CONSTRAINTS"
            actual = expected

        let ``drop without CASCADE CONSTRAINTS``() =
            let expected = Drop(Table(Ident [Name("PRICES")]), false, true)
            let actual = runParser Ddl.Parser.pdrop "DROP TABLE PRICES PURGE"
            actual = expected

        let ``drop with CASCADE CONSTRAINTS and PURGE``() =
            let expected = Drop(Table(Ident [Name("PRICES")]), true, true)
            let actual = runParser Ddl.Parser.pdrop "DROP TABLE PRICES CASCADE CONSTRAINTS PURGE"
            actual = expected
    
    [<Test.Runner.TestFixture>]
    module Create = 

        let ``create with ident and body``() =
            let expected = Create(Role(Ident [Name "APP_SUPPORT"]), false, [Body("NOT IDENTIFIED")])
            let actual = runParser Ddl.Parser.pcreate "CREATE ROLE APP_SUPPORT NOT IDENTIFIED" 
            actual = expected

        let ``create or replace with ident and body``() =
            let expected = Create(Role(Ident [Name "APP_SUPPORT"]), true, [Body("NOT IDENTIFIED")])
            let actual = runParser Ddl.Parser.pcreate "CREATE OR REPLACE ROLE APP_SUPPORT NOT IDENTIFIED" 
            actual = expected

        let ``type with body``() =
            let expected = Create(Type(Ident [Name "FCMG"; Name "PVT"]), true, [Body("""AS
  OBJECT
  (
    PERIOD     NUMBER (2) ,
    DATA_VALUE NUMBER (20,5) ) FINAL""")])
            let actual = 
                runParser Ddl.Parser.pcreate """CREATE OR REPLACE TYPE FCMG.PVT
AS
  OBJECT
  (
    PERIOD     NUMBER (2) ,
    DATA_VALUE NUMBER (20,5) ) FINAL ;"""
            actual = expected

    [<Test.Runner.TestFixture>]
    module Grant = 

        let ``grant with body``() = 
             let expected = Grant([Body("CREATE SESSION TO CONTRACTS")])
             let actual = runParser Ddl.Parser.pgrant """GRANT
CREATE SESSION TO CONTRACTS ;"""
             actual = expected

    [<Test.Runner.TestFixture>]
    module Comments = 

        let ``can handle inline comments --``() = 
            let expected = [Create(Table(Ident [Name("PRICES")]), false, [Comment("this is my comment")])]
            let actual = Ddl.Parser.parse  "CREATE TABLE PRICES --this is my comment"
            actual = expected

        let ``can handle inline comments /**/``() = 
            let expected = [Create(Table(Ident [Name("PRICES")]), false, [Comment("this is my comment")])]
            let actual = Ddl.Parser.parse "CREATE TABLE PRICES /*this is my comment*/"
            actual = expected

        let ``can handle inline comments /**/ multiline``() = 
            let expected = [
                    Comment """*****
*
*   Project :   Foo bar baz Quux
*
*****""";
                    Create (Table (Ident [Name "PRICES;"]),false,[])]
            let actual = Ddl.Parser.parse """
/******
*
*   Project :   Foo bar baz Quux
*
******/
CREATE TABLE PRICES;
""" 
            printfn "%A" expected
            printfn "%A" actual
            actual = expected

    [<Test.Runner.TestFixture>]
    module Statement = 

        let ``multiple statements``() =
            let expected = 2
            let actual = 
                Ddl.Parser.parse """CREATE USER Y IDENTIFIED BY PROFILE X ACCOUNT UNLOCK; GRANT CREATE SESSION TO Y ;"""
            actual.Length = expected

        let ``mulitple statements with /``() = 
            let expected = 2
            let actual =
                Ddl.Parser.parse """CREATE OR REPLACE TYPE X.RV_MESSAGE IS VARRAY ( 500 ) OF X.RV_FIELD ;
/
GRANT EXECUTE ON X.RV_MESSAGE TO PUBLIC ;"""
            actual.Length = expected



Test.Runner.run()


