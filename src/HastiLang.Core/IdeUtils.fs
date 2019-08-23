namespace HastiLang.Core.Ide

open Parsers
open BaseTypes

module IdeUtils =
    open Parsers
    let RunParser = Parsers.test


module CodeRunner =
    open InterpreterStates
    open System.Collections.Generic

    let rec eval (expr:HstExpression) (session:Session) =
        let inline checkEq a b = if a = b then Ok (Boolean HstTrue) else Ok (Boolean HstFalse)
        match expr with
        | ExpValue a -> Ok a
        | BindingExpr  a ->
            if session.bindings.ContainsKey a then
                let instr = session.bindings.[a].exps |> List.last
                match instr with
                | Value x -> Ok x
                | _ -> Error "خطا در پایان بدنه انقیاد"
            else Error "انقیادی با نام مورد نظر وجود نداشت"
        | ExpOpCall c ->
            match c.op with
            | Operator "+" ->
                match (eval c.exp1 session, eval c.exp2 session) with
                    | (Ok (Number j), Ok (Number k)) ->
                        match (j, k ) with
                        | (Float m , Float n) -> m + n |> (Float >> Number >> Ok)
                        | (Integer m, Integer n) -> m + n |> (Integer >> Number >> Ok)
                        | (Float m, Integer n) -> m + (float n) |> (Float >> Number >> Ok)
                        | (Integer m, Float n) -> (float m) + n |> (Float >> Number >> Ok)
                    | (Ok (String s1), Ok (String s2)) -> s1 + s2 |> (String >> Ok)
                    | _ -> Error "مقدار جمع غیر قابل محاسبه است"

            | Operator "*" ->
                match (eval c.exp1 session, eval c.exp2 session ) with
                    | (Ok (Number j), Ok (Number k)) ->
                        match (j, k ) with
                        | (Float m , Float n) -> m * n |> (Float >> Number >> Ok)
                        | (Integer m, Integer n) -> m * n |> (Integer >> Number >> Ok)
                        | (Float m, Integer n) -> m * (float n) |> (Float >> Number >> Ok)
                        | (Integer m, Float n) -> (float m) * n |> (Float >> Number >> Ok)
                    | Ok (Number j), Ok (String s) | Ok (String s), Ok (Number j) ->
                        match j with
                        | Float f -> String.replicate (int f) s |> (String >> Ok)
                        | Integer i -> String.replicate i s |> (String >> Ok)

                    | _ -> Error "مقدار ضرب غیر قابل محاسبه است"

            | Operator "%" ->
                 match (eval c.exp1 session, eval c.exp2 session ) with
                     | (Ok (Number j), Ok (Number k)) ->
                         match (j, k ) with
                         | (Float m , Float n) -> m % n |> (Float >> Number >> Ok)
                         | (Integer m, Integer n) -> m % n |> (Integer >> Number >> Ok)
                         | (Float m, Integer n) -> m % (float n) |> (Float >> Number >> Ok)
                         | (Integer m, Float n) -> (float m) % n |> (Float >> Number >> Ok)
                     | _ -> Error "مقدار باقی مانده غیر قابل محاسبه است"

            | Operator "==" ->
                match (eval c.exp1 session, eval c.exp2 session) with
                    | (Ok (Number j), Ok (Number k)) ->
                        match (j, k ) with
                        | (Float m , Float n) -> checkEq m n
                        | (Integer m, Integer n) -> checkEq m n
                        | (Float m, Integer n) -> Error "نوع صحیح با اعشاری هم خوانی ندارد"
                        | (Integer m, Float n) -> Error "نوع صحیح با اعشاری هم خوانی ندارد"
                    | (Ok (String s1), Ok (String s2)) -> checkEq s1 s2
                    | (Ok (Boolean b1), Ok (Boolean b2)) -> checkEq b1 b2
                    | _ -> Error "برابری غیر قابل محاسبه است"

            | _ -> Error "اپراتور تعریف نشده"

    let evalBool (expr:HstExpression) (session:Session) =
        match eval expr session with
        | Ok (Boolean hb) -> match hb with HstTrue -> Ok true | HstFalse -> Ok false
        | Error s -> Error (sprintf "%s (باید یک عبارت با نتیجه بولی قرار بگیرد))" s)
        | _ -> Error "باید یک عبارت با نتیجه بولی قرار بگیرد"


    let createSession() =
        {
            definedTypes = Dictionary<HstIdentifier, HstType>()
            bindings = Dictionary<HstIdentifier, HstBinding>()
        }

    let statementPrint (stmt:HstExpression) session =
        match eval stmt session with
        | Ok x ->
            match x with
            | Number x -> x.ToString()
            | String x -> x
            | Boolean x -> if x = HstTrue then "درست" else "غلط"
            | RecordInstance _ -> "نوع:رکورد"
            | Unit -> "واحد"
        | Error e -> sprintf "خطا در محاسبه (%s)" e

    let rec exec (stmts:HstStatement list) (prints:List<string>) (session:Session) =
        for s in stmts do
            match s with
            | PrintStatement p -> prints.Add (statementPrint p session)
            | Binding b -> session.bindings.Add (b.identifier, b)
            | IfStmt ifs ->
                match evalBool ifs.cond session with
                | Ok true -> (exec ifs.thendo prints session) |> ignore
                | Error x -> prints.Add x
                | _ -> ()
            | IfElseStmt ifs ->
                match evalBool ifs.cond session with
                | Ok true ->  (exec ifs.thendo prints session) |> ignore
                | Ok false -> (exec ifs.elsedo prints session) |> ignore
                | Error x -> prints.Add x
            | Loop x -> let lpFunc = loopExecutator x.body prints session x.boundIdf
                        match (x.fromIndex, x.toIndex) with
                        | (Integer a, Integer b) -> lpFunc a        b
                        | (Float a, Integer b)   -> lpFunc (int a) b
                        | (Integer a, Float b)   -> lpFunc a       (int b)
                        | (Float a, Float b)     -> lpFunc (int a) (int b)
            | FuncCall f -> let funcBinding = session.bindings.[f.id]
                            execFunc funcBinding prints session f.callArgs
            | _ -> ()

        prints

    and loopExecutator body prints session idf a b =
        let hasIdf = session.bindings.ContainsKey idf
        let oldBinding = if hasIdf then Some (session.bindings.[idf]) else None
        if hasIdf then session.bindings.Remove idf |> ignore
        let createValueBinding idf a = { identifier = idf
                                         args = None
                                         bindingType = Atomic (TypeName "عدد")
                                         exps = [a |> (Integer >> Number >> Value)]
                                       }
        session.bindings.Add (idf, createValueBinding idf a)
        for i = a to b do
            let newValue = createValueBinding idf i
            session.bindings.[idf] <- newValue
            (exec body prints session) |> ignore

        match oldBinding with
        | None -> session.bindings.Remove idf |> ignore
        | Some x -> session.bindings.[idf] <- x

    and execFunc funcBinding prints session (callArgs:HstCallArg list) =
        match funcBinding.args with
        | None -> prints.Add "آرگومانی برای اجرای تابع پاس داده نشده بود"
        | Some ar when ar.Length <> callArgs.Length -> prints.Add "آزگومان های تابع با تعداد آرگومان های درخواستی مطابقت نداشت. در این نسخه اعمال جزیی پشتیبانی نمیشود"
        | Some binds -> 
            let zipArgsAndIdfs = binds |> List.zip callArgs
            let createSessionByCallArgs() =
                let bs = Dictionary<HstIdentifier, HstBinding>()
                zipArgsAndIdfs |> List.iter (fun (arg, idf) ->
                    match arg with
                    | CallValue x -> bs.Add (idf, { identifier  = idf
                                                    args = None
                                                    bindingType = Atomic (TypeName "Dynamic")
                                                    exps = [Value x] })
                    | CallIdf a -> 
                        if session.bindings.ContainsKey a then
                            let instr = session.bindings.[a].exps |> List.last
                            match instr with
                            | Value x -> bs.Add (idf, { identifier  = idf
                                                        args = None
                                                        bindingType = Atomic (TypeName "Dynamic")
                                                        exps = [Value x] })
                            | _ -> prints.Add "خطا در پایان بدنه انقیاد"
                         else prints.Add "انقیادی با نام مورد نظر وجود نداشت"
                     
                )
                { definedTypes = Dictionary<HstIdentifier, HstType>()
                  bindings = bs}

            let tempSession = createSessionByCallArgs()

            exec funcBinding.exps prints tempSession |> ignore