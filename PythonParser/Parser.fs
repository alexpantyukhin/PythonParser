
namespace PythonParser

open System
open Types

module Parser =

    [<Literal>]
    let DEF = "def"

    [<Literal>]
    let CLASS = "class"

    [<Literal>]
    let IF = "if"

    [<Literal>]
    let ELSE = "else"

    [<Literal>]
    let ELIF = "elif"

    [<Literal>]
    let FROM = "from"
    
    [<Literal>]
    let IMPORT = "import"

    [<Literal>]
    let ASYNC = "async"

    [<Literal>]
    let NOTATION = "\"\"\""
    let indentation = "    "

    let openBrackets = ['('; '{'; '[']
    let closeBrackets = [')'; '}'; ']']
    let brackets = openBrackets @ closeBrackets

    let trim (str: string) =
        str.Trim()

    let cutLeft (length: int) (str: string) =
        str.Substring(length)

    let isEmpty (str: string) =
        str.Length = 0

    let cutRightFrom (from: int) (str: string) =
        str.Substring(0, from)

    let cleanLineFromComments (line : string) : string =
        // Simple way for now
        let commentSymbol = line.IndexOf("#")
        if commentSymbol = -1 then
            line
        else
            cutRightFrom commentSymbol line
    
    let rec firstOrSeparatorPosition(str: string, currIndex: int, bracketNumber: int): int = 
        if currIndex >= str.Length then
            str.Length
        else
            match str.[currIndex] with
            | ']' -> firstOrSeparatorPosition (str, currIndex + 1, bracketNumber - 1)
            | '[' -> firstOrSeparatorPosition (str, currIndex + 1, bracketNumber + 1)
            | '|' when bracketNumber = 0 -> currIndex
            | _ -> firstOrSeparatorPosition (str, currIndex + 1, bracketNumber)

    let rec splitTypesByComma(str: string, currIndex: int, prevPosition: int, bracketNumber: int): string list = 
        if currIndex >= str.Length then
            [str.Substring(prevPosition, currIndex - prevPosition)]
        else
            match str.[currIndex] with
            | ']' -> splitTypesByComma (str, currIndex + 1, prevPosition, bracketNumber - 1)
            | '[' -> splitTypesByComma (str, currIndex + 1, prevPosition, bracketNumber + 1)
            | ',' when bracketNumber = 0 ->
                [str.Substring(prevPosition, currIndex - prevPosition)]
                @
                splitTypesByComma(str, currIndex + 1, currIndex + 1, bracketNumber)
            | _ -> splitTypesByComma (str, currIndex + 1, prevPosition, bracketNumber)

    let rec parseType (typeString: string) : Type =
        let trimmedTypeString = trim typeString
        let orSeparatorPosition = firstOrSeparatorPosition (trimmedTypeString , 0, 0)
        if orSeparatorPosition = trimmedTypeString .Length then
            let leftBracket = trimmedTypeString .IndexOf("[")
            if leftBracket > -1 then
                let name = 
                    trimmedTypeString
                    |> cutRightFrom leftBracket
                    |> trim
                let innerTypesStr = trimmedTypeString.Substring(leftBracket + 1, trimmedTypeString.Length - leftBracket - 2)
                let innerTypesMap =
                    splitTypesByComma(innerTypesStr, 0, 0, 0)
                    |> List.map (trim >> parseType)

                CompositionType (name, innerTypesMap)

            else
                SimpleType trimmedTypeString
        else
            let headType =
                trimmedTypeString
                |> trim
                |> cutRightFrom orSeparatorPosition
                |> parseType

            let tailType =
                trimmedTypeString
                |> trim
                |> cutLeft (orSeparatorPosition + 1)
                |> parseType

            OrType ([headType] @ [ tailType ])

    let parseTypePart (funPartType: string) : Type =
        let trimmed = trim funPartType
        
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
        | position when position = argString.Length -> [ argString |> trim |> parseArg ]
        | position ->
            [
            argString
            |> cutRightFrom position
            |> trim
            |> parseArg]
            @
            (
            argString
            |> cutLeft (position + 1)
            |> trim
            |> parseArgs)
            
    let rec findMultipleLineBlock(lines: string[], currIndex: int, startIndex: int, startString: string, endString: string) : (string list) * int =
        let line =
            if currIndex = startIndex then
                lines.[currIndex] |> cutLeft (lines.[currIndex].IndexOf(startString) + 1)
            else
                lines.[currIndex]

        let cleanLine = cleanLineFromComments line

        let closedBracketIndex = cleanLine.IndexOf(endString)
        if closedBracketIndex > -1 then
            [(cleanLine
            |> cutRightFrom closedBracketIndex
            |> trim)], currIndex
        else
             let gathered, index = findMultipleLineBlock(lines, currIndex + 1, startIndex, startString, endString)
             [cleanLine] @ gathered, index

    let gatherArgsFromMultipleLines(lines: string[], currIndex: int) : string * int =
        let lines, index = findMultipleLineBlock(lines, currIndex, currIndex, "(", ")")
        String.Join("", lines), index

    let parseFunc (lines: string[], currIndex: int) : FunctionDef * int =
        let collected_args, index = gatherArgsFromMultipleLines(lines, currIndex)
        let line =
            lines.[currIndex]
            |> cleanLineFromComments

        match line.Split([| '(' |]) with
        | [| name; _ |] ->
            let namePartFunc = trim name
            
            let isAsync, name =
                if namePartFunc.StartsWith(ASYNC) then
                    true, namePartFunc
                            |> cutLeft ASYNC.Length
                            |> trim
                            |> cutLeft DEF.Length
                            |> trim
                else
                    false, namePartFunc
                            |> trim
                            |> cutLeft DEF.Length
                            |> trim
                            
            let returnType, resIndex =
                match lines.[index].Split("->") with
                | [| _; funPartType |] ->
                    let lines, rindex = findMultipleLineBlock(lines, index, index, ">", ":")
                    let joinedType = String.Join("", lines)

                    parseTypePart(joinedType), rindex
                | _ -> SimpleType "", index

            {
                FunctionDef.Name = name
                Async = isAsync
                Args = parseArgs(collected_args)
                Type = returnType;
            }, resIndex + 1

    let parseClassDefinition (classHead: string) : string * string list =
        let withoutClass = 
            classHead
            |> cleanLineFromComments
            |> trim 
            |> cutLeft CLASS.Length
            |> trim 

        match withoutClass.Split([| '('; ')'; ':' |]) with
        | [| name; inheritList; _ ; _|] -> name, inheritList.Split(",") |> Seq.toList
        | [| name; _ |] -> name, []
        
    let rec getNextBracketPosition(lines: string[], currIndex: int, currCursor: int, isString: bool) : int * int =
        let line = lines.[currIndex]
        if currCursor >= line.Length then
            getNextBracketPosition(lines, currIndex + 1, 0, isString)
        else
            let char = line.[currCursor]
            if char = '\"' then
                getNextBracketPosition(lines, currIndex, currCursor + 1, not isString)
            else
                if (not isString) && List.contains char brackets then
                    currIndex, currCursor
                else
                    getNextBracketPosition(lines, currIndex, currCursor + 1, isString)
            
    let rec parseVariableValue(lines: string[], currIndex: int, currCursor: int, openBracketsNumber: int): int * int =
        let line = lines.[currIndex]
        if openBracketsNumber = 0 then
            let nextOpenBracket = line.IndexOfAny(List.toArray openBrackets, currCursor) 
            if nextOpenBracket = -1 then
                if line.EndsWith("\"") || line.EndsWith(",") then
                    parseVariableValue(lines, currIndex + 1, 0, 0)
                else
                    currIndex, currCursor
            else
                if List.contains line.[nextOpenBracket] openBrackets then
                    parseVariableValue(lines, currIndex, nextOpenBracket + 1, openBracketsNumber + 1)
                else
                    currIndex, nextOpenBracket
        else
            let nextBracketIndex, nextBracketCursor = getNextBracketPosition(lines, currIndex, currCursor, false)
            if List.contains lines.[nextBracketIndex].[nextBracketCursor] openBrackets then
                parseVariableValue(lines, nextBracketIndex, nextBracketCursor + 1, openBracketsNumber + 1)
            else
                parseVariableValue(lines, nextBracketIndex, nextBracketCursor + 1, openBracketsNumber - 1)

    let parseVariable (lines: string[], currIndex: int) : VariableDef * int =
        let line =
            lines.[currIndex]
            |> cleanLineFromComments
        match line.Split([| ':' |]) with
        | [| name; typeStr  |] -> {VariableDef.Name = trim name; Type = parseType typeStr} , currIndex + 1
        | [| _; |] ->
            let equalitySignIndex = line.IndexOf("=")
            let currIndex, _ = parseVariableValue(lines, currIndex, equalitySignIndex + 1, 0)
            {VariableDef.Name = (line |> cutRightFrom equalitySignIndex |> trim) ; Type = SimpleType ""} , currIndex + 1
            
    let parseNotation(lines: string[], currIndex: int) : NotationDef * int =
        let lines, index = findMultipleLineBlock(lines, currIndex, currIndex, NOTATION, NOTATION)
        {NotationDef.Notation = lines}, index + 1

    let rec getAllImportFromMultipleLines(lines: string[], currIndex: int): string * int =
        let lines, index = findMultipleLineBlock(lines, currIndex, currIndex, "(", ")")
        String.Join("", lines), index

    let parseFrom(lines: string[], currIndex: int): FromDef * int =
        let left =
            lines.[currIndex]
            |> cleanLineFromComments
            |> cutLeft FROM.Length
            |> trim

        let firstWhiteSpace = left.IndexOf(" ")
        let fromValue = cutRightFrom firstWhiteSpace left
        let leftWithoutName =
            left
            |> cutLeft firstWhiteSpace
            |> trim
            |> cutLeft IMPORT.Length
            |> trim
        
        let openBracketIndex = leftWithoutName.IndexOf("(")
        let imports, index = 
            if openBracketIndex > -1 then
                getAllImportFromMultipleLines(lines, currIndex)
            else
                leftWithoutName, currIndex

        {
            FromDef.From = (trim fromValue)
            Items = imports.Split(",") |> Seq.toList |> List.map trim
        }, index + 1

    let rec parseElsePart (lines: string[], currIndex: int, currLevel: int) : ElseDef * int =
        if currIndex = lines.Length then
            ElseDef.Items [], currIndex
        else
            let elseLine =
                lines.[currIndex]
                |> cleanLineFromComments
            if (elseLine |> trim |> isEmpty) then
                parseElsePart(lines, currIndex + 1, currLevel)
            else if elseLine.StartsWith ((String.replicate (currLevel - 1) indentation) + ELSE) then
                let items, index = parseUnits (lines, currIndex + 1, currLevel)
                ElseDef.Items items, index
            else if elseLine.StartsWith ((String.replicate (currLevel - 1) indentation) + ELIF) then
                let ifDef, funcIndex = parseElif(lines, currIndex, currLevel) 
                ElseDef.IfDef ifDef, funcIndex 
            else 
                ElseDef.Items [], currIndex

    and parseElif(lines: string[], currIndex: int, currLevel: int): IfDef * int =
        parseCondition(ELIF, lines, currIndex, currLevel)

    and parseIf(lines: string[], currIndex: int, currLevel: int) : IfDef * int =
        parseCondition(IF, lines, currIndex, currLevel)

    and parseCondition(keyword: string, lines: string[], currIndex: int, currLevel: int) : IfDef * int = 
        let line =
            lines.[currIndex]
            |> cleanLineFromComments
            |> cutLeft keyword.Length
        match line.Split(":") with
        | [| condition; _ |] ->
            let thenItems, thenIndex = parseUnits(lines, currIndex + 1, currLevel)
            let elseItems, elseIndex = parseElsePart(lines, thenIndex, currLevel)

            { IfDef.Condition = trim condition; Then = thenItems; Else = elseItems }, elseIndex

    and parseClass (lines: string[], currIndex: int, currLevel: int): ClassDef * int = 
        let name, inherits = parseClassDefinition(lines.[currIndex])
        let classItems, index = parseUnits(lines, currIndex + 1, currLevel + 1)
        { ClassDef.Name = name; Inherits = inherits; Items = classItems } , index

    and parseUnits (lines: string[], currIndex: int, currLevel: int ): Unit list * int =
        if currIndex = lines.Length then
            [], currIndex
        else
            let line =
                lines.[currIndex]
                |> cleanLineFromComments

            let trimmedLine = trim line

            if (isEmpty trimmedLine) || trimmedLine.StartsWith("@") || trimmedLine.StartsWith("import") then
                parseUnits(lines, currIndex + 1, currLevel)
            else
                if (not (line.StartsWith (String.replicate currLevel indentation))) then
                    [], currIndex
                else
                    if trimmedLine.StartsWith(FROM + " ") then
                        let fromDef, nextIndex = parseFrom(lines, currIndex)
                        let moduleItems, index = parseUnits(lines, nextIndex, currLevel)
                        [ FromDef fromDef ] @ moduleItems, index
                    else if trimmedLine.StartsWith(CLASS + " ") then
                        let classDef, nextIndex = parseClass(lines, currIndex, currLevel)
                        let moduleItems, index = parseUnits(lines, nextIndex, currLevel)
                        [ ClassDef classDef ] @ moduleItems, index
                    else if trimmedLine.StartsWith(DEF + " ") || trimmedLine.StartsWith(ASYNC + " ") then
                        let func, nextIndex = parseFunc(lines, currIndex)
                        let moduleItems, index = parseUnits(lines, nextIndex, currLevel)
                        [ FunctionDef func] @ moduleItems, index
                    else if trimmedLine.StartsWith(IF + " ") then
                        let ifDef, nextIndex = parseIf(lines, currIndex, currLevel + 1)
                        let moduleItems, index = parseUnits(lines, nextIndex, currLevel)
                        [ IfDef ifDef] @ moduleItems, index
                    else if trimmedLine.StartsWith(NOTATION) then
                        let notationDef, nextIndex = parseNotation(lines, currIndex)
                        let moduleItems, index = parseUnits(lines, nextIndex, currLevel)
                        [ NotationDef notationDef] @ moduleItems, index
                    else
                        let variableDef, nextIndex = parseVariable(lines, currIndex)
                        let moduleItems, index = parseUnits(lines, nextIndex, currLevel)
                        [ VariableDef variableDef ] @ moduleItems, index
        
    let parseModule (source: string) : Module =
        let lines = source.Split("\n")
        let items, _ = parseUnits(lines, 0, 0)
        { Module.Items = items }
