
namespace PythonParser

module Parser =

    [<Literal>]
    let DEF = "def"

    [<Literal>]
    let CLASS = "class"

    [<Literal>]
    let IF = "if"

    let indentation = "    "

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
        
    type IfDef = {
        Condition: string
        ThenItems: ModuleItem list
        ElseItems: ModuleItem list
    }

    and ModuleItem =
    | FunctionDef of FunctionDef  
    | ClassDef of ClassDef 
    | VariableDef of VariableDef
    | IfDef of IfDef

    type Module = {
        Items: ModuleItem list 
    } 

    let trim (str: string) =
        str.Trim()

    let cutLeft (length: int) (str: string) =
        str.Substring(length)

    let isEmpty (str: string) =
        str.Length = 0

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
                |> cutLeft (orSeparatorPosition + 1)
                |> parseType

            OrType ([headType] @ [ tailType ])

    let parseTypePart (funPartType: string) : Type =
        let trimmed = funPartType |> trim
        
        if isEmpty trimmed then
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
            |> cutLeft (position + 1)
            |> trim
            |> parseArgs)
            
    let rec gatherArgsFromMultipleLines(lines: string[], currIndex: int, startIndex: int) : string * int =
        let line =
            if currIndex = startIndex then
                lines.[currIndex] |> cutLeft (lines.[currIndex].IndexOf("(") + 1)
            else
                lines.[currIndex]

        if line.Contains(")") then
            (line
            |> (fun x -> x.Substring(0, line.IndexOf(")")))
            |> trim), currIndex
        else
             let gathered, index = gatherArgsFromMultipleLines(lines, currIndex + 1, startIndex)
             line + gathered, index

    let parseFunc (lines: string[], currIndex: int) : FunctionDef * int =
        let collected_args, index = gatherArgsFromMultipleLines(lines, currIndex, currIndex)
        let line = lines.[currIndex]

        match line.Split([| '(' |]) with
        | [| name; _ |] ->
            match lines.[index].Split("->") with
            | [| _; funPartType |] ->
                let nameFunc = name
                                |> trim
                                |> cutLeft DEF.Length
                                |> trim

                {
                    FunctionDef.Name = nameFunc
                    Args = parseArgs(collected_args)
                    Type = parseTypePart(funPartType);
                }, index + 1

    let parseClassDefinition (classHead: string) : (string * string list) =
        let withoutClass = 
            classHead 
            |> trim 
            |> cutLeft CLASS.Length
            |> trim 

        match withoutClass.Split([| '('; ')'; ':' |]) with
        | [| name; inheritList; _ ; _|] -> name, inheritList.Split(",") |> Seq.toList
        | [| name; _ |] -> name, []
        
    let parseVariable (lines: string[], currIndex: int) : VariableDef * int =
        let line = lines.[currIndex]
        match line.Split([| ':' |]) with
        | [| name; typeStr  |] -> {VariableDef.Name = name.Trim(); Type = parseType(typeStr)} , currIndex + 1
        | [| _; |] ->
            match line.Split([| '=' |]) with
            | [| name; _  |] -> {VariableDef.Name = name.Trim(); Type = SimpleType ""} , currIndex + 1

    let rec parseClassItems(lines: string[], currIndex: int, currLevel: int) : ClassItem list * int =
        if currIndex = lines.Length then 
            [], currIndex
        else
            let line = lines.[currIndex]
            let trimmed = trim line

            if (isEmpty trimmed) then
                parseClassItems(lines, currIndex + 1, currLevel)
            else
                if not (line.StartsWith(String.replicate currLevel indentation)) then
                    [], currIndex
                else
                    if trimmed.StartsWith(DEF) then
                        let func, funcIndex = parseFunc(lines, currIndex)
                        let moduleItems, index = parseClassItems(lines, funcIndex, currLevel)
                        [ ClassItem.FunctionDef func] @ moduleItems, index
                    else
                        let variableDef, nextIndex = parseVariable(lines, currIndex)
                        let moduleItems, index = parseClassItems(lines, nextIndex, currLevel)
                        [ ClassItem.VariableDef variableDef ] @ moduleItems, index

    let parseClass (lines: string[], currIndex: int, currLevel: int): ClassDef * int = 
        let name, inherits = parseClassDefinition(lines.[currIndex])
        let classItems, index = parseClassItems(lines, currIndex + 1, currLevel + 1)
        { ClassDef.Name = name; Inherits = inherits; Items = classItems } , index
        
    let rec parseElsePart (lines: string[], currIndex: int, currLevel: int) : ModuleItem list * int =
        if currIndex = lines.Length then
            [], currIndex
        else
            let elseLine = lines.[currIndex]
            if (elseLine |> trim |> isEmpty) then
                parseElsePart(lines, currIndex + 1, currLevel)
            else if elseLine.StartsWith ((String.replicate (currLevel - 1) indentation) + "else") then
                parseModuleItems (lines, currIndex + 1, currLevel)
            else
                [], currIndex
        
    and parseIf(lines: string[], currIndex: int, currLevel: int) : IfDef * int =
        let line =
            lines.[currIndex]
            |> cutLeft IF.Length
        match line.Split(":") with
        | [| condition; _ |] ->
            let thenItems, thenIndex = parseModuleItems(lines, currIndex + 1, currLevel)
            let elseItems, elseIndex = parseElsePart(lines, thenIndex, currLevel)

            {IfDef.Condition = trim condition; ThenItems = thenItems; ElseItems = elseItems }, elseIndex

    and parseModuleItems (lines: string[], currIndex: int, currLevel: int ): ModuleItem list * int =
        if currIndex = lines.Length then
            [], currIndex
        else
            let line = lines.[currIndex]
            if (not (line.StartsWith (String.replicate currLevel indentation))) then
                [], currIndex
            else
                let trimmedLine = trim line
                if (isEmpty trimmedLine) || trimmedLine.StartsWith("@") || trimmedLine.StartsWith("import") || trimmedLine.StartsWith("from") then
                    parseModuleItems(lines, currIndex + 1, currLevel)
                else
                    if trimmedLine.StartsWith(CLASS) then
                        let classDef, classIndex = parseClass(lines, currIndex, currLevel)
                        let moduleItems, index = parseModuleItems(lines, classIndex, currLevel)
                        [ ModuleItem.ClassDef classDef ] @ moduleItems, index
                    else if trimmedLine.StartsWith(DEF) then
                        let func, funcIndex = parseFunc(lines, currIndex)
                        let moduleItems, index = parseModuleItems(lines, funcIndex, currLevel)
                        [ ModuleItem.FunctionDef func] @ moduleItems, index
                    else if trimmedLine.StartsWith(IF) then
                        let ifDef, funcIndex = parseIf(lines, currIndex, currLevel + 1)
                        let moduleItems, index = parseModuleItems(lines, funcIndex, currLevel)
                        [ ModuleItem.IfDef ifDef] @ moduleItems, index
                    else
                        let variableDef, nextIndex = parseVariable(lines, currIndex)
                        let moduleItems, index = parseModuleItems(lines, nextIndex, currLevel)
                        [ ModuleItem.VariableDef variableDef ] @ moduleItems, index
    
    let parseModule (source: string) : Module =
        let lines = source.Split("\n")
        let items, _ = parseModuleItems(lines, 0, 0)
        { Module.Items = items }
