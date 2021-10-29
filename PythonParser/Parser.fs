namespace PythonParser

module Parser =

    type Type =
    | SimpleType of string
    | OrType of Type list
    | CompositionType of string * Type list

    type Argument = 
        {
            Name: string
            Type: Type
        }

    type FunctionDef =
        {
            Name: string
            Args: Argument list
            Type: Type
        }

    let rec parseType (typeString: string) : Type =
        match typeString.Split([| '['; ']' |]) with
        | [| mainName; innerTypes; _|] ->
            CompositionType
                (mainName.Trim(),
                innerTypes.Split(",")
                 |> Seq.toList
                 |> List.map (fun x -> x.Trim())
                 |> List.map parseType)
        | value ->
            match typeString.Split([| '|' |]) with
            | lst when lst.Length > 1 ->
                OrType
                 (lst
                 |> Seq.toList
                 |> List.map (fun x -> x.Trim())
                 |> List.map parseType)

            | _ -> SimpleType typeString

    let parseTypePart (funPartType: string) : Type =
        funPartType.Trim().Substring("->".Length).Trim()
        |> parseType

    let parseArg (arg : string) : Argument =
        match arg.Split([| ':'; '=' |]) with
        | [| var1; var2 |] | [| var1; var2; _ |] -> { Argument.Name = var1.Trim(); Type = parseType(var2.Trim()) }

    let firstArgSeparatorPosition (argString: string) : int =
        let mutable brackets = 0
        let mutable position = -1
        
        for i in seq { 0 .. argString.Length - 1 } do
            let char = argString.[i]
            brackets <- brackets + (if char = '[' then 1 elif char = ']' then -1 else 0)

            if char = ',' && brackets = 0 && position = -1 then
                position <- i

        if position = -1 then argString.Length else position

    let rec parseArgs (argString : string) : Argument list =
        match firstArgSeparatorPosition(argString) with
        | position when position = argString.Length -> [ argString.Trim() |> parseArg ]
        | position ->
            [(argString.Substring(0, position).Trim()
            |> parseArg)]
            @
            (argString.Substring(position + 1).Trim()
            |> parseArgs)

    let parseFunc (funcSignature : string) : FunctionDef =
        match funcSignature.Split([| '('; ')' |]) with
        | [| name; argsPart; funPartType |] -> {
            FunctionDef.Name = name.Trim()
            Args = parseArgs(argsPart)
            Type = parseTypePart(funPartType);
        }
