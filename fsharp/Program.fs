// Learn more about F# at http://fsharp.org

open System


let rec repl () : unit =
    printf "user> "

    match Console.ReadLine () with
    | "#quit" | "#q" -> ()
    | cmd -> 
        printfn "%s" cmd
        repl ()


[<EntryPoint>]
let main argv =
    repl ()
    0 // return an integer exit code
