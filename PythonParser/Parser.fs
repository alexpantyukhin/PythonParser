
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
        
    type VariableDef =
        {
            Name: string
            Type: Type
        }

    type ClassDef = 
        {
            Name: string
            Inherits: string list
            Funcs: FunctionDef list
        }

    type ModuleItem =
    | FunctionDef of FunctionDef  
    | ClassDef of ClassDef 
    | VariableDef of VariableDef

    type Module = {
        Items: ModuleItem list 
    } 

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

    let rec getClassFuncs(lines: string[], currIndex: int) : FunctionDef list * int =
        if currIndex = lines.Length then 
            [], currIndex
        else
            let line = lines.[currIndex]

            if ( line.Trim().Length = 0) then
                getClassFuncs(lines, currIndex + 1)
            else
                if not (line.StartsWith("  ")) && not (line.StartsWith(" ")) then
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
        let name, inherits = parseClassDefinition(lines.[currIndex])
        let classItems, index = getClassFuncs(lines, currIndex + 1)
        { ClassDef.Name = name; Inherits = inherits; Funcs = classItems } , index
        
    let parseVariable (lines: string[], currIndex: int) : VariableDef * int =
        let line = lines.[currIndex]
        match line.Split([| ':'; ']' |]) with
        | [| name; typeStr  |] -> {VariableDef.Name = name.Trim(); Type = parseType(typeStr)} , currIndex + 1

    let rec parseModuleItems (lines: string[], currIndex: int ): ModuleItem list * int =
        if currIndex = lines.Length then
            [], currIndex
        else
            let line = lines.[currIndex]

            if ( line.Trim().Length = 0) then
                parseModuleItems(lines, currIndex + 1)
            else
                if line.StartsWith("class") then
                    let classDef, classIndex = parseClass(lines, currIndex)
                    let moduleItems, index = parseModuleItems(lines, classIndex)
                    [ ModuleItem.ClassDef classDef ] @ moduleItems, index
                else if line.StartsWith("def") then
                    let func = parseFunc(line)
                    let moduleItems, index = parseModuleItems(lines, currIndex + 1)
                    [ ModuleItem.FunctionDef func] @ moduleItems, index
                else
                    let variableDef, nextIndex = parseVariable(lines, currIndex)
                    let moudleItems, index = parseModuleItems(lines, nextIndex)
                    [ ModuleItem.VariableDef variableDef ] @ moudleItems, index
    
    let parseModule (source: string) : Module =
        let lines = source.Split("\n")
        let items, _ = parseModuleItems(lines, 0)
        {Module.Items = items}
