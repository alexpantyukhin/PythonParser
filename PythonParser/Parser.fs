
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

    type ClassDef = 
        {
            Name: string
            Inherits: string list
            Funcs: FunctionDef list
        }

    type ModuleItem =
    | FunctionDef
    | ClassDef

    type Module = ModuleItem list

    let trim (str: string) =
        str.Trim()

    let rec parseType (typeString: string) : Type =
        match typeString.Split([| '['; ']' |]) with
        | [| mainName; innerTypes; _|] ->
            CompositionType
                (mainName.Trim(),
                innerTypes.Split(",")
                 |> Seq.toList
                 |> List.map trim
                 |> List.map parseType)
        | _ ->
            match typeString.Split([| '|' |]) with
            | lst when lst.Length > 1 ->
                OrType
                 (lst
                 |> Seq.toList
                 |> List.map trim
                 |> List.map parseType)

            | _ -> SimpleType typeString

    let parseTypePart (funPartType: string) : Type =
        let trimmed = funPartType |> trim
        
        if trimmed.Length = 0 then
            SimpleType ""
        else
            trimmed 
            |> (fun x -> x.Substring("->".Length))
            |> trim
            |> parseType

    let parseArg (arg : string) : Argument =
        match arg.Split([| ':'; '=' |]) with
        | [| var1; var2 |] | [| var1; var2; _ |] -> { Argument.Name = trim var1; Type = var2 |> trim |> parseType }
        | [| var1; |] -> { Argument.Name = trim var1; Type = SimpleType "" }

    let rec firstArgSeparatorPosition (argString: string, currIndex: int, bracketNumber: int) : int =
        if currIndex >= argString.Length then
            argString.Length
        else
            match argString.[currIndex] with
            | ']' -> firstArgSeparatorPosition (argString, currIndex + 1, bracketNumber - 1)
            | '[' -> firstArgSeparatorPosition (argString, currIndex + 1, bracketNumber + 1)
            | ',' when bracketNumber = 0 -> currIndex
            | _ -> firstArgSeparatorPosition (argString, currIndex + 1, bracketNumber)

    let rec parseArgs (argString : string) : Argument list =
        match firstArgSeparatorPosition(argString, 0, 0) with
        | position when position = argString.Length -> [ argString.Trim() |> parseArg ]
        | position ->
            [
            argString
            |> (fun x -> x.Substring(0, position))
            |> trim
            |> parseArg]
            @
            (
            argString
            |> (fun x -> x.Substring(position + 1))
            |> trim
            |> parseArgs)

    let parseFunc (funcSignature : string) : FunctionDef =
        match funcSignature.Split([| '('; ')' |]) with
        | [| name; argsPart; funPartType |] -> {
            FunctionDef.Name = name.Trim()
            Args = parseArgs(argsPart)
            Type = parseTypePart(funPartType);
        }

    let parseClassDefinition (classHead: string) : (string * string list) =
        let withoutClass = 
            classHead 
            |> trim 
            |> (fun x -> x.Substring("class".Length))
            |> trim 
        
        match withoutClass.Split([| '('; ')'; ':' |]) with
        | [| name; inheritList; _ ; _|] -> name, inheritList.Split(",") |> Seq.toList
        | [| name; _ |] -> name, []

    let rec getClassFuncs(lines: string[], currIndex: int) : (FunctionDef list * int) =
        if currIndex = lines.Length then 
            [], currIndex
        else
            let line = lines.[currIndex] 

            if ( line.Trim().Length = 0) ||  not (line.StartsWith("  ")) && not (line.StartsWith(" ")) then
                [], currIndex
            else
                let func = 
                    line
                    |> trim
                    |> (fun x -> x.Substring("def".Length))
                    |> trim
                    |> parseFunc

                let funcs, index = getClassFuncs(lines, currIndex + 1)
                [func] @ funcs, index

    let parseClass (lines: string[], currIndex: int): ClassDef * int = 
        let (name, inherits) = parseClassDefinition(lines.[currIndex])

        let classItems, index = getClassFuncs(lines, currIndex + 1)
        ({ClassDef.Name = name; Inherits = inherits; Funcs = classItems}) , index

    // let parseModule (source: string) : Module = 
    //     [ ({ModuleItem.ClassDef.Name = "Hello"; Inherits = []; Funcs=[]})]