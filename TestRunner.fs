namespace Test

module Runner =

    open System
    open FSharp.Reflection
    open System.Reflection

    type TestFixture() = 
        inherit Attribute()

    type TestResult =
        | Success of string
        | Failure of string
        | Errored of string * exn
        override x.ToString() = 
            match x with
            | Success n -> sprintf "Success: %s" n
            | Failure n -> sprintf "Failure: %s" n
            | Errored (n,e) -> sprintf "Error: %s %s" n e.Message

    type RunResult = {
         Failed : int
         Passed : int
         Errored : int
         Results : TestResult list
    }
    with
        static member Empty = { Failed = 0; Passed = 0; Errored = 0; Results = [] }

        member x.Failures =
            x.Results
            |> List.filter (function | Failure _ -> true | _ -> false)

        member x.Errors =
            x.Results
            |> List.filter (function | Errored _ -> true | _ -> false)

        member x.NonPassing =
            x.Results
            |> List.filter (function | Errored _ -> true | Failure _ -> true | _ -> false)

        static member (+) (a:RunResult, b:RunResult) = 
            { a with
                Failed = a.Failed + b.Failed
                Passed = a.Passed + b.Passed
                Errored = a.Errored + b.Errored
                Results = a.Results @ b.Results
            }
        
    let run() =
#if INTERACTIVE
        fsi.AddPrinter (fun (x:RunResult) -> 
            if List.isEmpty x.NonPassing
            then 
                sprintf 
                    "Test Results - Passed: %d, Failed: %d, Errored: %d" 
                    x.Passed 
                    x.Failed 
                    x.Errored
            else 
                sprintf     
                    "Test Results - Passed: %d, Failed: %d, Errored: %d\r\n[\r\n%s\r\n]" 
                    x.Passed 
                    x.Failed 
                    x.Errored
                    (x.NonPassing |> List.map (sprintf "%A") |> String.concat Environment.NewLine)
        )
#endif
        let sut = Assembly.GetExecutingAssembly()
        let testFixtureType = typeof<TestFixture>

        let tests =
            sut.GetTypes()
            |> Array.collect (fun (x:Type) -> 
                if x.CustomAttributes |> Seq.exists (fun a -> a.AttributeType = testFixtureType)
                then x.GetMembers()
                else [||]
            )
            |> Array.choose (function
                 | :? MethodInfo as mi ->
                    if (mi.ReturnParameter.ParameterType = typeof<bool>) && (mi.GetParameters() = [||])
                    then Some mi
                    else None
                 | _ -> None)
            |> List.ofArray

        let runTest state (test:MethodInfo) =
            let name = test.DeclaringType.Name + "." + test.Name
            try
               match unbox<bool> (test.Invoke(null, [||])) with
               | true -> { state with Passed = state.Passed + 1; Results = (Success name) :: state.Results }
               | false -> { state with Failed = state.Failed + 1; Results = (Failure name) :: state.Results }
            with e ->
               { state with Errored = state.Errored + 1; Results = (Errored(name, e)) :: state.Results }
                   
        
        let rec runTests state (tests:MethodInfo list) =
            match tests with
            | [] -> state
            | test :: t -> runTests (runTest state test) t        

        runTests RunResult.Empty tests