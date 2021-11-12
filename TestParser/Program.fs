// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

// Define a function to construct a message to print

open System
open System.IO
open PythonParser

let wrongParsed (filesPaths: string list): (string * Exception) list =
    filesPaths
    |> List.map (fun x ->
        let source = (File.ReadAllText x)
        try
            (Parser.parseModule source) |> ignore
            x, None
        with
        | :? Exception as ex -> x, Some(ex)
        )
    |> List.filter (fun (_, y) ->
                match y with
                | Some (y) -> true
                | _ -> false)
    |> List.map (fun (x, y) ->
                match y with
                | Some(y) -> (x, y))

[<EntryPoint>]
let main argv =
    let sourcePath = ".\\typeshed\\stdlib"
    let files =
     Directory.GetFiles(sourcePath, "*.pyi", SearchOption.AllDirectories)
     |> Seq.toList
    let wrongParsedFiles = wrongParsed(files)
    printfn "All: %s" (files.Length.ToString())
    printfn "Wrong: %s" (wrongParsedFiles.Length.ToString())
    0 // return an integer exit code