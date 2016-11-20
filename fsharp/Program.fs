// Learn more about F# at http://fsharp.org

open System

type Result<'a,'b> = OK of 'a | Error of 'b 

module Tokenize =

    open System.Text.RegularExpressions

    type Expr =
    | Number of value : int
    | Text of text : string
    | StrLiteral of value : string
    | List of exprs : Expr list
    | Whitespace

    with 
        override expr.ToString () =
            match expr with
            | Number value -> value.ToString()
            | Text text -> sprintf "\"%s\"" text
            | StrLiteral value -> value
            | Whitespace -> ""
            | List es ->
                es |> Seq.choose (function Whitespace -> None | x -> Some x) |> Seq.map (fun x->x.ToString()) |> String.concat " " |> sprintf "(%s)"


    let tryRegex pattern s =
        Regex pattern
        |> fun x -> x.Match s
        |> fun m -> 
            if m.Success then Some m.Length
            else None

    let (|IsChar|_|) ch str = if not <| String.IsNullOrEmpty str && str.[0] = ch then Some ch else None
    let (|IsCharT|_|) ch str = if not <| String.IsNullOrEmpty str && str.[0] = ch then Some <| str.Substring 1 else None
    
    let (|IsRegex|_|) p s = tryRegex p s |> Option.map (fun i -> (s.Substring(0, i), i))

    let rec (|IsNumber|_|) = function
    | IsRegex "^[-]{0,1}[0-9]+" (str, len) -> 
        match Int32.TryParse str with
        | (true, value) -> (Number value, len) |> Some
        | (false, _) -> None
    | _ -> None

    let rec (|IsText|_|) = function
    | IsRegex "^\"[^\"]*\"+" (str, len) -> (Text (str.Substring(1, str.Length - 2)), len) |> Some
    | _ -> None

    let (|IsStrLiteral|_|) = function
    | IsChar '+' ch 
    | IsChar '-' ch 
    | IsChar '/' ch 
    | IsChar '*' ch 
        -> Some (ch.ToString() |> StrLiteral, 1)
    | IsRegex "^[A-Za-z][A-Za-z0-9]*" (str, len) -> Some (str |> StrLiteral, len) 
    | _ -> None
    let (|IsWhitespace|_|) = function
    | IsRegex "^[\s]+" (_, len) -> Some len
    | _ -> None

    let tokenize (s:string) : Expr list=
        let rec tokenize' s = 
            match s with
            | IsWhitespace len -> (Whitespace, len) |> Some
            | IsNumber (expr, len)
            | IsText (expr, len)
            | IsStrLiteral (expr, len)
            | IsList '(' ')' (expr, len)
                 -> (expr, len) |> Some
            | _ -> None
        and (|IsList|_|) startChar endChar s = 
            let (|T|) s =
                let rec t' s curr =  
                    match tokenize' s with
                    | None -> curr |> List.rev, s
                    | Some (expr, len) -> t' (s.Substring len) (expr::curr) 
                t' s []
            match s with
            | IsCharT startChar (T (es, IsCharT endChar rem)) -> (List es, s.Length - rem.Length) |> Some
            | _ -> None

        s |> List.unfold (fun str -> if String.IsNullOrEmpty str then None 
                                     else tokenize' str |> Option.map (fun (token, len)-> token, str.Substring len))

let rec repl () : unit =
    printf "user> "

    match Console.ReadLine () with
    | "#quit" | "#q" -> ()
    | cmd -> 
        try
            Tokenize.tokenize cmd 
            |> Seq.choose (function Tokenize.Expr.Whitespace -> None | x -> Some x)
            |> Seq.map (fun x->x.ToString())
            |> String.concat " " 
            |> printfn "%s"
        with e -> printfn "Exception %O" e
        repl ()


[<EntryPoint>]
let main argv =
    repl ()
    0 // return an integer exit code
