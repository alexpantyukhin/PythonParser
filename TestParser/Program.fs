// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

// Define a function to construct a message to print

open System
open System.IO
open PythonParser
open Types

type parsedFiles = (string * Module option * Exception option) list

let parseFiles(filesPaths: string list) : parsedFiles = 
    filesPaths
    |> List.map (fun x ->
        try
            let source = (File.ReadAllText x)
            let parsedModule = Parser.parseModule source
            x, Some(parsedModule), None
        with
        | :? Exception as ex -> x, None, Some(ex)
        )

let validFiles (parsedFiles) : (string * Module) list = 
    parsedFiles
    |> List.filter (fun (_, m, _) ->
                match m with
                | Some (m) -> true
                | _ -> false)
    |> List.map (fun (f, m, _) ->
                match m with
                | Some(m) -> (f, m))

let wrongFiles (parsedFiles) : (string * Exception) list = 
    parsedFiles
    |> List.filter (fun (_, _, e) ->
                match e with
                | Some (e) -> true
                | _ -> false)
    |> List.map (fun (f, _, e) ->
                match e with
                | Some(e) -> (f, e))

[<EntryPoint>]
let main argv =
    let sourcePath = ".\\typeshed\\stdlib"
    let files =
        Directory.GetFiles(sourcePath, "*.pyi", SearchOption.AllDirectories)
        |> Seq.toList
    
    let parsedFilesList = parseFiles files
    let wrongFilesList = wrongFiles parsedFilesList
    let validFilesList = validFiles parsedFilesList

    printfn "All: %d" (parsedFilesList.Length)
    printfn "Valid: %d" (validFilesList.Length)
    printfn "Wrong: %d" (wrongFilesList.Length)
    0 // return an integer exit code