// Learn more about F# at http://fsharp.org

open System

type Result<'a,'b> = OK of 'a | Error of 'b 
type Expr =
    | Number of value : int
    | Text of text : string
    | StrLiteral of value : string
    | List of exprs : Expr list
    | Exprs of exprs : Expr list
    | Vector of exprs : Expr list
    | HashMap of exprs : Expr list
    | Function of args : string list * expr:Expr

    with override expr.ToString () =
            let getInnerExpression es = es |> Seq.map (fun x->x.ToString()) |> String.concat " "
            match expr with
            | Number value -> value.ToString()
            | Text text -> sprintf "\"%s\"" text
            | StrLiteral value -> value
            | Exprs es -> getInnerExpression es |> sprintf "%s" 
            | List es -> getInnerExpression es |> sprintf "(%s)" 
            | Vector es -> getInnerExpression es |> sprintf "[%s]"
            | HashMap es -> getInnerExpression es |> sprintf "{%s}"
            | Function _ -> "#<function>"

type Env = Map<string, Expr>

module Eval =
    let mathOp = 
        ["+", (+)
         "-", (-)
         "*", (*)
         "/", (/)] |> Map
    let (|CanEvalMath|_|) es = 
        let (|AllAreNumbers|_|) es = if List.isEmpty es |> not && (List.forall (function Number x -> true | _ -> false) es) then 
                                          es |> List.map (fun (Number x) -> x) |> Some 
                                     else None 
        match es with
        | StrLiteral ch :: AllAreNumbers num -> 
            match mathOp.TryFind ch with
            | Some op -> num |> List.reduce op |> (Number >> Some)
            | None -> None
            
        | _ -> None
    let rec eval (env:Env) es : (Expr*Env) = 
        let evalNested ctor es : (Expr*Env)=
            
            let (|FuncArgs|_|) es = 
                let rec args curr = function
                | StrLiteral arg :: xs -> args (arg :: curr) xs
                | [] ->Some curr
                | _ -> None
                args [] es

            let (|Args|) es = 
                let rec args env = function
                | StrLiteral arg :: value :: xs -> 
                    let value = eval env value |> fst
                    let env = env.Add (arg, value)
                    args env xs
                | [] -> env
                | x -> failwithf "Wrong number of attributes (leftover %A)" x
                args env es

            let (|CanEvalToConst|_|) e =
                match eval env e |> fst with
                | Number _
                | Function _
                | Text _ as e -> Some e
                | _ -> None

            match es with
            | StrLiteral "def!" :: StrLiteral name :: [CanEvalToConst value] ->
                let value = eval env value |> fst
                value, (env.Add (name, value))
            | StrLiteral "let*" :: List (Args localEnv) :: [expr]
            | StrLiteral "let*" :: Vector (Args localEnv) :: [expr]  ->                
                eval localEnv expr |> fst, env
            | StrLiteral "fn*" :: Vector (FuncArgs args) :: [expr] ->
                let value = Function (args, expr)
                value, env
            | _ -> 
                let mutable env = env
                let es = [for e in es do 
                            let expr, envirement = eval env e
                            env<-envirement
                            yield expr ]
                match es with 
                | CanEvalMath es -> es, env
                | [Function ([], e)] -> e, env
                | Function (a::args, e) :: p :: ps ->
                    let localEnv = env.Add (a, p)
                    ([Function(args, eval localEnv e |> fst)] @ ps |> List |> eval localEnv |> fst), env
                | _ -> ctor es, env
            
        match es with
        | Number _
        | Function _ 
        | Text _ as x -> x, env
        | StrLiteral x as e ->
            match env.TryFind x with
            | Some e -> e, env
            | None -> e, env
        | Exprs es -> evalNested Exprs es
        | HashMap es -> evalNested HashMap es
        | List es -> evalNested List es
        | Vector es -> evalNested Vector es

module Tokenize =
    [<AutoOpen>]
    module private Tokenize =
        type Token =
        | Number of value : int
        | Text of text : string
        | StrLiteral of value : string
        | List of exprs : Token list
        | Vector of exprs : Token list
        | HashMap of exprs :Token list
        | Whitespace

        let rec toExpr = function
        | [] -> []
        | Whitespace :: ts -> toExpr ts
        | t::ts ->
            let t = match t with
                    | Number s -> Expr.Number s
                    | Text s -> Expr.Text s
                    | StrLiteral s -> Expr.StrLiteral s
                    | List s -> s |> toExpr |> Expr.List
                    | Vector s -> s |> toExpr |> Expr.Vector
                    | HashMap s -> s |> toExpr |> Expr.HashMap
                    | Whitespace -> failwith "Whitespace is unexpected here."
            t :: toExpr ts

        open System.Text.RegularExpressions
        let tryRegex pattern s =
            Regex pattern
            |> fun x -> x.Match s
            |> fun m -> 
                if m.Success then Some m.Length else None

        let (|IsChar|_|) ch str = if not <| String.IsNullOrEmpty str && str.[0] = ch then Some <| str.Substring 1 else None
        let (|IsStartingWith|_|) startsWith str = if not <| String.IsNullOrEmpty str && str.StartsWith startsWith then Some <| str.Substring startsWith.Length else None
        let (|IsRegex|_|) p s = tryRegex p s |> Option.map (fun i -> (s.Substring(0, i), i))

        let rec (|IsNumber|_|) = function
        | IsRegex "^[-]{0,1}[0-9]+" (str, len) -> 
            match Int32.TryParse str with
            | true, value -> (Number value, len) |> Some
            | false, _ -> None
        | _ -> None

        let rec (|IsText|_|) = function
        | IsRegex "^\"([^\"\\\\]|\\\\\")*\"" (str, len) -> (Text (str.Substring(1, str.Length - 2)), len) |> Some
        | _ -> None

        let (|IsStrLiteral|_|) = function
        | IsRegex "^[^\s\[\]{}('\"`,;)\r\n\s]+" (str, len) -> Some (str |> StrLiteral, len) 
        | _ -> None
        let (|IsWhitespace|_|) = function
        | IsRegex "^[\s,]+" (_, len) -> Some len
        | _ -> None



    let tokenize (s:string) : Expr=
        let rec tokenize' s = 
            let (|IsNested|_|) s =
                let (|NestedExpression|_|) (s:string) =
                    match tokenize' s with
                    | None -> None
                    | Some (expr, len) -> Some (expr, s.Substring len, len) 
                let (|NestedExpresions|) =
                    let rec t' curr =  function
                    | NestedExpression (expr, remaining, _) -> t' (expr::curr) remaining                 
                    | s -> curr |> List.rev, s
                    t' []
                let (|IsBracket|_|) startChar endChar expr s =
                    match s with
                    | IsChar startChar (NestedExpresions (es, IsChar endChar rem)) -> (expr es, s.Length - rem.Length) |> Some
                    | _ -> None
                let (|IsExtendable|_|) startsWith str s =
                    match s with
                    | IsStartingWith startsWith (NestedExpression (e, _, len)) ->  (List [StrLiteral str; e], len + 1) |> Some
                    | _ -> None
                
                match s with
                | IsBracket '(' ')' List (expr, len)
                | IsBracket '{' '}' HashMap (expr, len)
                | IsBracket '[' ']' Vector (expr, len)
                | IsExtendable "'" "quote" (expr, len)         
                | IsExtendable "~@" "splice-unquote" (expr, len)           
                | IsExtendable "~" "unquote" (expr, len)
                | IsExtendable "`" "quasiquote" (expr, len)          
                | IsExtendable "@" "deref" (expr, len)                
                    -> Some (expr, len)
                | IsStartingWith "^" (NestedExpression (vector, (NestedExpresions(es, rem)), _)) -> 
                    (List ([StrLiteral "with-meta"] @ es @ [vector]), s.Length - rem.Length) |> Some
                |_ -> None

            match s with
            | IsWhitespace len -> (Whitespace, len) |> Some
            | IsNumber (expr, len)
            | IsText (expr, len)
            | IsNested (expr, len)      
            | IsStrLiteral (expr, len)
                 -> (expr, len) |> Some
            | _ -> None

        s |> List.unfold (fun str -> if String.IsNullOrEmpty str then None 
                                     else tokenize' str |> Option.map (fun (token, len)-> token, str.Substring len))
        |> toExpr
        |> Exprs                   
let rec repl env : unit =
    printf "user> "

    match Console.ReadLine () with
    | "#quit" | "#q" -> ()
    | cmd ->
        let mutable env2 = env
        try
            let expr, env = Tokenize.tokenize cmd |> Eval.eval env
            env2<-env
            expr.ToString() |> printfn "%s"
        with e -> printfn "Exception %O" e
        repl env2       


[<EntryPoint>]
let main argv =
    repl Map.empty
    0 // return an integer exit code
