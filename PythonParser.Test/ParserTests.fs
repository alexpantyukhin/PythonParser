module PythonParser.Test

open NUnit.Framework
open Parser

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ParseSingleArgTests () =
    let result = parseFunc("func(arg1: string) -> float")
    Assert.AreEqual(
        {
            FunctionDef.Name = "func";
            Type = SimpleType "float"
            Args = [
                {
                    Argument.Name = "arg1";
                    Type = SimpleType "string"
                }
            ]
        }, result)

[<Test>]
let ParseManyArgsTests () =
    let result = parseFunc("func ( arg1: string , arg2: int ) -> float")
    Assert.AreEqual(
        {
            FunctionDef.Name = "func";
            Type = SimpleType "float"
            Args = [
                {
                    Argument.Name = "arg1";
                    Type = SimpleType "string"
                };
                {
                    Argument.Name = "arg2";
                    Type = SimpleType "int"
                }
            ]
        }, result)


[<Test>]
let ParseArgWithEnumTests () =
    let result = parseFunc("func ( arg1: string | int, arg2: int ) -> float")
    Assert.AreEqual(
        {
            FunctionDef.Name = "func";
            Type = SimpleType "float"
            Args = [
                {
                    Argument.Name = "arg1";
                    Type = OrType [ SimpleType "string"; SimpleType "int" ]
                 };
                 {
                     Argument.Name = "arg2";
                     Type = SimpleType "int"
                 }
            ]
        }, result)
    
[<Test>]
let ParseArgWithTupleTests () =
    let result = parseFunc("func ( arg1: tuple[int, string], arg2: int ) -> float")
    Assert.AreEqual(
        {
            FunctionDef.Name = "func";
            Type = SimpleType "float"
            Args = [
                {
                    Argument.Name = "arg1";
                    Type = CompositionType ("tuple", [ SimpleType "int"; SimpleType "string" ])
                 };
                 {
                     Argument.Name = "arg2";
                     Type = SimpleType "int"
                 }
            ]
        }, result)

[<Test>]
let ParseClass () =
    let source = "class MyClass:
    def __init__(self, arg: int)
    def next_method(self, arg1: string, arg2: string)
"
    let result, _ = parseClass(source.Split("\n"), 0)

    Assert.AreEqual(
        {            
            ClassDef.Name = "MyClass";
            Inherits = []
            Funcs = 
            [
                {
                    FunctionDef.Name = "__init__";
                    Type = SimpleType ""
                    Args = [
                        {
                            Argument.Name = "self";
                            Type = SimpleType ""
                         };
                         {
                             Argument.Name = "arg";
                             Type = SimpleType "int"
                         }
                    ]
                };
                {
                    FunctionDef.Name = "next_method";
                    Type = SimpleType ""
                    Args = [
                         {
                            Argument.Name = "self";
                            Type = SimpleType ""
                         };
                         {
                             Argument.Name = "arg1";
                             Type = SimpleType "string"
                         };
                         {
                             Argument.Name = "arg2";
                             Type = SimpleType "string"
                         }
                    ]
                }
            ]
        }, result)
    
[<Test>]
let ParseClassWithInherits () =
    let source ="""class MyClass(Basic):
    def __init__(self, arg: int) -> None

    def next_method(self, arg1: string, arg2: string) -> int

"""
    let result, _ = parseClass(source.Split("\n"), 0)

    Assert.AreEqual(
        {            
            ClassDef.Name = "MyClass";
            Inherits = [ "Basic" ]
            Funcs = 
            [
                {
                    FunctionDef.Name = "__init__";
                    Type = SimpleType "None"
                    Args = [
                        {
                            Argument.Name = "self";
                            Type = SimpleType ""
                         };
                         {
                             Argument.Name = "arg";
                             Type = SimpleType "int"
                         }
                    ]
                };
                {
                    FunctionDef.Name = "next_method";
                    Type = SimpleType "int"
                    Args = [
                         {
                            Argument.Name = "self";
                            Type = SimpleType ""
                         };
                         {
                             Argument.Name = "arg1";
                             Type = SimpleType "string"
                         };
                         {
                             Argument.Name = "arg2";
                             Type = SimpleType "string"
                         }
                    ]
                }
            ]
        }, result)
    
    
[<Test>]
let ParseModule () =
    let source ="""
def func(arg1: string, arg2: int) -> float

class MyClass(Basic):
    def __init__(self, arg: int) -> None

    def next_method(self, arg1: string, arg2: string) -> int

"""
    let result = parseModule(source)

    Assert.AreEqual(
        {            
            ClassDef.Name = "MyClass";
            Inherits = [ "Basic" ]
            Funcs = 
            [
                {
                    FunctionDef.Name = "__init__";
                    Type = SimpleType "None"
                    Args = [
                        {
                            Argument.Name = "self";
                            Type = SimpleType ""
                         };
                         {
                             Argument.Name = "arg";
                             Type = SimpleType "int"
                         }
                    ]
                };
                {
                    FunctionDef.Name = "next_method";
                    Type = SimpleType "int"
                    Args = [
                         {
                            Argument.Name = "self";
                            Type = SimpleType ""
                         };
                         {
                             Argument.Name = "arg1";
                             Type = SimpleType "string"
                         };
                         {
                             Argument.Name = "arg2";
                             Type = SimpleType "string"
                         }
                    ]
                }
            ]
        }, result)