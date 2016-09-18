#I "packages/FParsec/lib/net40-client/"
#r "FParsecCS.dll"
#r "FParsec.dll"

#nowarn "40"

module Ddl =

    open FParsec
    open FParsec.Primitives
    open FParsec.CharParsers
    open System.Collections.Generic

    module Ast = 

        type Ident = 
            | Name of string 
            | Ident of Ident list
        
        type Parameter = 
            | Parameter of string * string
        
        type Size = 
            | K of int
            | M of int
            | G of int
            | T of int
            | Unlimited
        
        type AutoExtend =
            | On of Size option * max:Size option
            | Off

        type FileSpec = 
            | FileSpec of string list * Size option * reuse:bool * AutoExtend option

        type Logging = 
            | Logging
            | NoLogging
        
        type Parallel = 
            | Parallel of int
            | NoParallel

        type Cache = 
            | Cache
            | NoCache
        
        type BufferPool = 
            | Keep 
            | Recycle 
            | Default

        type Storage = 
            | Initial of Size
            | Next of Size
            | MinExtents of int
            | MaxExtents of int option
            | PctIncrease of int
            | FreeLists of int
            | FreeListGroups of int
            | Optimal of Size option
            | BufferPool of BufferPool

        type PhysicalAttributes = 
            | PctFree of int
            | PctUsed of int
            | IniTrans of int
            | Storage of Storage list
        
        type AllocateExent = 
            | DataFile of string
            | Instance of int
            | Size of Size

        type IndexClause = 
            | Name of Ident
            | Create of Statement
            | Properties of string

        and ConstraintState = 
            | Deferrable of bool
            | InitiallyImmediate
            | InitiallyDeferred
            | Enable
            | Disable
            | Validate
            | NoValidate
            | Rely
            | NoRely
            | Index of IndexClause
            | ExceptionInto of Ident 

        and Constraint =
            | Inline of Ident * Constraint list * ConstraintState list
            | OutOfLine of Ident * Constraint list * ConstraintState list
            | InlineRef of Constraint list
            | OutOfLineRef of Constraint list
            | Nullable of bool
            | Unique of Ident list
            | PrimaryKey of Ident list
            | ForeignKey of Ident list * ReferenceClause
            | Check of string
            | Reference of ReferenceClause
            | Scope of Ident * scopeFor:Ident
            | WithRowId of rowidRef:Ident
            | Constraint of Ident * Constraint list * ReferenceClause * ConstraintState
        
        and DeleteClause = 
            | Cascade 
            | SetNull

        and ReferenceClause = 
            | Reference of Ident * Ident option * DeleteClause option
        
        and Clauses = 
            | AllocateExent of AllocateExent list
            | Constraint of Constraint  
            | DeallocateUnused of Size option
            | FileSpec of FileSpec
            | Logging of Logging
            | Parallel of Parallel
            | PhysicalAttributes of PhysicalAttributes
            | Storage of Storage list
            | Size of Size
            | Cache of Cache
        
        and SchemaObject = 
            | Cluster
            | Constraint
            | Link
            | Trigger
            | Database 
            | Dimensions
            | ExternalProcedureLibrary
            | Function
            | IndexTables
            | Index
            | IndexType
            | Java
            | MaterializedView
            | MaterializedViewLog
            | ObjectTables
            | ObjectTypes
            | ObjectViews
            | Package
            | PackageBody
            | Procedure
            | Role
            | Session
            | Sequence
            | System
            | Synonym
            | Table
            | Type
            | User
            | View

        and Statement = 
            | Alter of SchemaObject * Ident * Clauses list
            | Create of replace:bool * SchemaObject * Ident
            | Comment of string
            | Body of string  

     module Parser = 

        open System
        open Ast

        let lineCommentStr = "--"
        let openMultilineCommentStr, closeMultilineCommentStr = "/*", "*/"
        let symbols = "[]\"'()*,.".ToCharArray()
        let quote = skipStringCI "\"" <|> skipStringCI "'"
        let quotedStr = ((skipChar 'N' >>.  quote) <|> quote) >>. manyCharsTill anyChar quote
        let identifierString = many1Satisfy (fun c -> not(System.Char.IsWhiteSpace c) && isNoneOf symbols c)

        let keywords = [
            "DROP"; "CREATE"; "REPLACE"; "VIEW"; "TABLE"; "FUNCTION"; "PROCEDURE"; "PACKAGE"; "ALTER";
            "CLUSTER"; "DATABASE"; "SIZE"; "CACHE"; "BODY"; "BEGIN"; "END"; "PARALLEL"; "NULL"; "NOT"
            "VIEW"; "UNIQUE"; "CASCADE"; "INITIALLY"; "ENABLE"; "DISABLE"; "VALIDATE"; "NOVALIDATE"; "NORELY"
            "RELY"; "DATAFILE"; "INSTANCE"; "ROWID"; "DEFERABLE"; "INITIAL"; "MINEXTENTS"; "MAXEXTENTS"; "NEXT"
            "PCTINCREASE"; "FREELIST"; "FREELIST GROUPS"; "BUFFERPOOL"; "DEALLOCATE"; "KEEP"; "UNUSED"
        ]

        
        let lineComment : Parser<string, _> = 
            spaces >>. pstring lineCommentStr >>. restOfLine true .>> spaces

        let rec multilineComment =
            spaces >>. between
                (pstring openMultilineCommentStr)
                (pstring closeMultilineCommentStr)
                (charsTillString closeMultilineCommentStr false System.Int32.MaxValue)
            .>> spaces

        let pcomment = 
            spaces >>. choice [ lineComment; multilineComment ] .>> spaces
            |>> Comment
        
        let ws = 
            choice [ 
                spaces
                lineComment |>> ignore
                multilineComment |>> ignore
            ]

        let strWS s = ws >>. pstring s .>> ws
        let skipStrWS s = ws >>. skipStringCI s .>> ws
        let betweenStr a b p = between (strWS a) (strWS b) p
        let choiceWs cs = ws >>. choice (cs |> Seq.map (fun x -> ws >>. x .>> ws)) .>> ws 
        let keywordSet = new HashSet<string>(keywords)
        let statementTerminators = new HashSet<_>(['/';';'])
        let isKeyword (kw:string) = keywordSet.Contains(kw.ToUpper())
        let keyword (kw:string) = 
            ws >>. skipStringCI kw >>. ws
            
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
        
        let pstatement, pstatementref = createParserForwardedToRef()

        let pterminators = 
            statementTerminators
            |> Seq.map (fun x -> ws >>. pchar x .>> ws)
            |> choice

        let pident = 
            ws >>. (sepBy (identifier |>> Ident.Name) (pchar '.') |>> Ident) .>> ws
        
        let pparameter = 
            ws >>. identifier .>> ws .>> skipStringCI "IN" .>> ws .>>. identifier .>> ws
            |>> Parameter

        let pparameterList = 
            ws >>. between (pchar '(') (pchar ')') (sepBy pparameter (ws >>. pchar ',' .>> ws)) .>> ws 

        let pSize = 
            let mag = 
                choice [
                   pint32 .>> ws .>> pstringCI "K" |>> K 
                   pint32 .>> ws .>> pstringCI "M" |>> M 
                   pint32 .>> ws .>> pstringCI "G" |>> G 
                   pint32 .>> ws .>> pstringCI "T" |>> T 
                ]
            ws .>> pstringCI "SIZE" .>> ws >>. mag

        let pParallel = 
            choiceWs [ 
                pstringCI "PARALLEL" .>> ws >>. pint32 |>> Parallel.Parallel
                pstringCI "NOPARALLEL" >>% NoParallel
            ]

        let pCache = 
            choiceWs [ 
                pstringCI "CACHE" >>% Cache.Cache
                pstringCI "NOCACHE" >>% NoCache
            ] 

        let plogging = 
            choiceWs [ 
                pstringCI "LOGGING" >>% Logging.Logging
                pstringCI "NOLOGGING" >>% NoLogging
            ] 

        let pBufferPool = 
            choiceWs [ 
                pstringCI "KEEP" >>% BufferPool.Keep
                pstringCI "RECYCLE" >>% BufferPool.Recycle
                pstringCI "DEFAULT" >>% BufferPool.Default
            ] 

        let pStorageClauses =    
            choiceWs [
                pstringCI "INITIAL" >>. ws >>. pSize |>> Storage.Initial
                pstringCI "NEXT" >>. ws >>. pSize |>> Storage.Next
                pstringCI "MINEXTENTS" >>. ws >>. pint32 |>> Storage.MinExtents
                pstringCI "MAXEXTENTS" >>. ws >>. choice [ pstringCI "UNLIMITED" >>% None; pint32 |>> Some ] |>> Storage.MaxExtents
                pstringCI "PCTINCREASE" >>. ws >>. pint32 |>> Storage.PctIncrease
                pstringCI "FREELIST" >>. ws >>. pint32 |>> Storage.FreeLists
                pstringCI "FREELIST GROUPS" >>. ws >>. pint32 |>> Storage.FreeListGroups
                pstringCI "OPTIMAL" >>. ws >>. choice [ pstringCI "NULL" >>% None; pSize |>> Some ] |>> Storage.Optimal
                pstringCI "BUFFER_POOL" >>. ws >>. pBufferPool |>> Storage.BufferPool
            ]

        let pStorage = 
            ws >>. pstringCI "STORAGE" >>. ws >>. betweenStr "(" ")" (many pStorageClauses)
        
        let pAllocateExent = 
            let clause = 
                choiceWs [
                    pSize |>> AllocateExent.Size
                    pstringCI "DATAFILE" >>. ws >>. quotedStr |>> AllocateExent.DataFile
                    pstringCI "INSTANCE" >>. ws >>. pint32 |>> AllocateExent.Instance
                ]
            ws >>. pstringCI "ALLOCATE EXTENT" >>. ws >>. betweenStr "(" ")" (many clause)
            |>> AllocateExent

        let pdeallocateUnused = 
            ws >>. pstringCI "DEALLOCATE UNUSED" >>. ws >>. opt (pstringCI "KEEP" >>. pSize)
            |>> DeallocateUnused
        
        let pautoextend = 
            let maxSize = 
                pstringCI "MAXSIZE" .>> ws >>. choice [
                    pstringCI "UNLIMITED" >>% None
                    pSize |>> Some
                ] .>> ws
            let next = 
                (opt (pstring "NEXT" .>> ws >>. pSize))
            let ae = 
                choice [
                    pstring "ON" >>. ws >>. next .>> ws .>>. maxSize |>> On
                    pstring "OFF" >>% Off
                ]
            ws >>. pstringCI "AUTOEXTEND" >>. ws >>. ae

        let pfileSpec = 
            let reuse = 
                opt (pstringCI "REUSE")
                |>> (function | Some _ -> true | _ -> false)
            
            pipe4
                (ws >>. (many quotedStr <|> betweenStr "(" ")"  (many quotedStr)))
                (ws >>. (opt pSize))
                (ws >>. reuse)
                (ws >>. (opt pautoextend))
                (fun a b c d -> FileSpec.FileSpec(a,b,c,d))
            .>> ws
            |>> FileSpec
        
        let pphysicalAttributes = 
            let clause =    
                choiceWs [
                    pstringCI "PCTFREE" >>. ws >>. pint32 |>> PctFree
                    pstringCI "PCTUSED" >>. ws >>. pint32 |>> PctUsed
                    pstringCI "INITRANS" >>. ws >>. pint32 |>> IniTrans
                    pStorage |>> PhysicalAttributes.Storage 
                ]
            
            ws >>. (many clause) .>> ws
        
        let punique = 
            choiceWs [
                pstringCI "UNIQUE" |>> (fun _ -> Unique [])
                pstringCI "UNIQUE" .>> ws >>. (sepBy1 pident (pchar ',')) |>> Unique 
            ]

        let pprimaryKey = 
            choiceWs [
                pstringCI "PRIMARY KEY" |>> (fun _ -> PrimaryKey [])
                pstringCI "PRIMARY KEY" .>> ws >>. (sepBy1 pident (pchar ',')) |>> PrimaryKey 
            ] 
        
    

        let pindexClause = 
            let globalPartitionBy = 
                pstringCI "GLOBAL PARTITION BY" >>. ws 
                >>. choiceWs [
                    pstringCI "RANGE" >>. ws >>. betweenStr "(" ")" (sepBy1 pident (pchar ',')) .>> ws 
                ]
            let indexProperties = 
                choiceWs [
                    
                ]
            ws >>. pstringCI "USING INDEX" >>. ws 
            >>. choice [
                pident |>> IndexClause.Name
                betweenStr "(" ")" pstatement |>> IndexClause.Create

            ]

        let pdeleteClause = 
            choiceWs [
                pstringCI "CASCADE" >>% Cascade
                pstringCI "SET NULL" >>% SetNull
            ]

        let preferenceClause = 
             ws 
             >>. pstringCI "REFERENCES"
             .>> ws 
             >>. pident 
             .>> ws 
             .>>. (opt (betweenStr "(" ")" pident))
             .>> ws
             .>>. (opt (pstringCI "ON DELETE" .>> ws >>. pdeleteClause)) 
             |>> (fun ((a,b),c) -> Reference(a,b,c))
        
                
        let pforiegnKey = 
            pstringCI "FOREIGN KEY" .>> ws >>. (sepBy1 pident (pchar ',')) .>> ws .>>. preferenceClause .>> ws
            |>> ForeignKey

        let pconstraintClauses = 
            choiceWs [ 
                opt (pstringCI "NOT") .>> ws .>> pstringCI "NULL" |>> (function None -> Nullable true | Some _ -> Nullable false)
                punique
                pprimaryKey
                pforiegnKey
                preferenceClause |>> Constraint.Reference
                pstringCI "CHECK" .>> ws >>. betweenStr "(" ")" (anyString Int32.MaxValue) |>> Check
            ]
        
        let pconstraintState = 
            choiceWs [
               opt (pstringCI "NOT") .>> ws .>> pstringCI "DEFERRABLE" |>> (function None -> Deferrable true | Some _ -> Deferrable false)
               pstringCI "INITIALLY" >>. ws .>> pstringCI "IMMEDIATE" >>% InitiallyImmediate
               pstringCI "INITIALLY" >>. ws .>> pstringCI "DEFERRED" >>% InitiallyDeferred
               pstringCI "ENABLE" >>% Enable
               pstringCI "DISABLE" >>% Disable
               pstringCI "VALIDATE" >>% Validate
               pstringCI "NOVALIDATE" >>% NoValidate
               pstringCI "RELY" >>% Rely
               pstringCI "NORELY" >>% NoRely
               pindexClause |>> ConstraintState.Index
               pexceptionClause |>> ConstraintState.ExceptionInto
            ]

        let pinlineConstraint = 
            ws >>. pstringCI "CONSTRAINT" .>> ws >>. pident .>> ws .>>. (many pconstraintClauses) .>> ws .>>. (many pconstraintState) .>> ws
            |>> (fun ((a,b),c) -> Inline(a,b,c)) 

        let pConstraint = 
        

        let pClauses = 
            ws >>. choice [
                pStorage |>> Storage
                pAllocateExent
                pdeallocateUnused
                pfileSpec
                plogging |>> Logging
                pParallel |>> Clauses.Parallel
            ] .>> ws
    
        let pbody = 
            ws >>. many1Satisfy (statementTerminators.Contains >> not) .>> ws |>> (fun x -> (x.Trim())) 

        let pschemaObject = 
            let ent name target =
                ws >>. skipStringCI name >>% target 
            ws >>. choice [
                ent "VIEW" View
                ent "MATERIALIZED VIEW" MaterializedView
                ent "TABLE" Table
                ent "PACKAGE" Package
                ent "PROCEDURE" Procedure
                ent "FUNCTION" Function
                ent "ROLE" Role
                ent "SESSION" Session
                ent "SYSTEM" System
                ent "SEQUENCE" Sequence
                ent "SYNONYM" Synonym
                ent "DATABASE LINK" Link
                ent "CLUSTER" Cluster
                ent "USER" User
                ent "TYPE" Type
            ] .>> ws
        
        let palter =
            ws >>. pstringCI "ALTER" .>> ws >>. pschemaObject .>> ws .>>. pident .>> ws .>>. many pClauses .>> ws 
            |>> (fun ((so, ident), clauses) -> Alter(so, ident, clauses)) 

        let pcreate = 
            let pcreateOrReplace = 
                (skipStringCI "CREATE OR REPLACE" >>% true) <|> (skipStringCI "CREATE" >>% false)
            ws >>. pcreateOrReplace .>> ws .>>. pschemaObject .>> ws .>>. pident .>> ws
            |>> (fun ((replace,so), name) -> Create(replace, so, name))

        do pstatementref := 
            ws >>. choice [
                pcomment
                pcreate
            ] .>> ws
        
        let ddlParser = 
            sepEndBy pstatement  (many pterminators)

        let parse (str:string) =
            match run ddlParser (str.Trim()) with
            | Success(r,_,_) -> r
            | Failure(msg, err,_) -> failwithf "Failed to parse %s'" msg
