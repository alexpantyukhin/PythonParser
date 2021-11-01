
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
        
    type ClassItem =
        | FunctionDef of FunctionDef  
        | VariableDef of VariableDef

    type ClassDef = 
        {
            Name: string
            Inherits: string list
            Items: ClassItem list
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
        
    let rec firstOrSeparatorPosition(str: string, currIndex: int, bracketNumber: int): int = 
        if currIndex >= str.Length then
            str.Length
        else
            match str.[currIndex] with
            | ']' -> firstOrSeparatorPosition (str, currIndex + 1, bracketNumber - 1)
            | '[' -> firstOrSeparatorPosition (str, currIndex + 1, bracketNumber + 1)
            | '|' when bracketNumber = 0 -> currIndex
            | _ -> firstOrSeparatorPosition (str, currIndex + 1, bracketNumber)

    let rec parseType (typeString: string) : Type =
        let orSeparatorPosition = firstOrSeparatorPosition (typeString, 0, 0)
        if orSeparatorPosition = typeString.Length then
            match typeString.Split([| '['; ']' |]) with
            | [| mainName; innerTypes; _|] | [| mainName; innerTypes;|]->
                CompositionType
                    (mainName.Trim(),
                    innerTypes.Split(",")
                     |> Seq.toList
                     |> List.map trim
                     |> List.map parseType)
            | _ -> SimpleType (typeString |> trim)
        else
            let headType =
                typeString
                |> trim
                |> (fun x -> x.Substring(0, orSeparatorPosition))
                |> parseType

            let tailType =
                typeString
                |> trim
                |> (fun x -> x.Substring(orSeparatorPosition + 1))
                |> parseType
                
            OrType ([headType] @ [ tailType ])

    let parseTypePart (funPartType: string) : Type =
        let trimmed = funPartType |> trim
        
        if trimmed.Length = 0 then
            SimpleType ""
        else
            trimmed 
            |> trim
            |> (fun x -> x.Split(":").[0])
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
            
    let rec gatherArgs(lines: string[], currIndex: int, startIndex: int) : string * int =
        let line =
            if currIndex = startIndex then
                lines.[currIndex].Substring(lines.[currIndex].IndexOf("(") + 1)
            else
                lines.[currIndex]
                
        if line.Contains(")") then
            (line
            |> (fun x -> x.Substring(0, line.IndexOf(")")))
            |> trim), currIndex
        else
             let gathered, index = gatherArgs(lines, currIndex + 1, startIndex)
             line + gathered, index

    let parseFunc (lines: string[], currIndex: int) : FunctionDef * int =
        let collected_args, index = gatherArgs(lines, currIndex, currIndex)
        let line = lines.[currIndex]

        match line.Split([| '(' |]) with
        | [| name; _ |] ->
            match lines.[index].Split("->") with
            | [| _; funPartType |] ->
                let nameFunc = name
                                |> trim
                                |> (fun x -> x.Substring("def".Length))
                                |> trim

                {
                    FunctionDef.Name = nameFunc
                    Args = parseArgs(collected_args)
                    Type = parseTypePart(funPartType);
                }, index + 1

    let parseFuncDefinition(lines: string[], currIndex: int): FunctionDef * int  = 
        parseFunc(lines, currIndex)

    let parseClassDefinition (classHead: string) : (string * string list) =
        let withoutClass = 
            classHead 
            |> trim 
            |> (fun x -> x.Substring("class".Length))
            |> trim 

        match withoutClass.Split([| '('; ')'; ':' |]) with
        | [| name; inheritList; _ ; _|] -> name, inheritList.Split(",") |> Seq.toList
        | [| name; _ |] -> name, []
        
    let parseVariable (lines: string[], currIndex: int) : VariableDef * int =
        let line = lines.[currIndex]
        match line.Split([| ':' |]) with
        | [| name; typeStr  |] -> {VariableDef.Name = name.Trim(); Type = parseType(typeStr)} , currIndex + 1

    let rec getClassItems(lines: string[], currIndex: int) : ClassItem list * int =
        if currIndex = lines.Length then 
            [], currIndex
        else
            let line = lines.[currIndex]

            if ( line.Trim().Length = 0) then
                getClassItems(lines, currIndex + 1)
            else
                if not (line.StartsWith("  ")) && not (line.StartsWith(" ")) then
                    [], currIndex
                else
                    let trimmed = line.Trim()

                    if trimmed.StartsWith("def") then
                        let func, funcIndex = parseFuncDefinition(lines, currIndex)
                        let moduleItems, index = getClassItems(lines, funcIndex)
                        [ ClassItem.FunctionDef func] @ moduleItems, index
                    else
                        let variableDef, nextIndex = parseVariable(lines, currIndex)
                        let moudleItems, index = getClassItems(lines, nextIndex)
                        [ ClassItem.VariableDef variableDef ] @ moudleItems, index

    let parseClass (lines: string[], currIndex: int): ClassDef * int = 
        let name, inherits = parseClassDefinition(lines.[currIndex])
        let classItems, index = getClassItems(lines, currIndex + 1)
        { ClassDef.Name = name; Inherits = inherits; Items = classItems } , index

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
                    let func, funcIndex = parseFuncDefinition(lines, currIndex)
                    let moduleItems, index = parseModuleItems(lines, funcIndex)
                    [ ModuleItem.FunctionDef func] @ moduleItems, index
                else
                    let variableDef, nextIndex = parseVariable(lines, currIndex)
                    let moudleItems, index = parseModuleItems(lines, nextIndex)
                    [ ModuleItem.VariableDef variableDef ] @ moudleItems, index
    
    let parseModule (source: string) : Module =
        let lines = source.Split("\n")
        let items, _ = parseModuleItems(lines, 0)
        {Module.Items = items}
