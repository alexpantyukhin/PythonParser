namespace Types

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
        Async: bool
        Type: Type
    }
    
type FromDef = {
    From: string
    Items: string list
}

type NotationDef = {
    Notation: string list
}

type VariableDef =
    {
        Name: string
        Type: Type
    }

type IfDef = {
    Condition: string
    Then: ThenDef
    Else: ElseDef
}

and ThenDef = Unit list

and ElseDef =
| IfDef of IfDef
| Items of Unit list

and ClassDef = {
    Name: string
    Inherits: string list
    Items: Unit list
}

and Unit =
| FunctionDef of FunctionDef  
| ClassDef of ClassDef 
| VariableDef of VariableDef
| IfDef of IfDef
| FromDef of FromDef
| NotationDef of NotationDef

type Module = {
    Items: Unit list 
}
