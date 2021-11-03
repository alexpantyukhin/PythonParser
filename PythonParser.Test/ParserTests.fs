module PythonParser.Test

open NUnit.Framework
open Parser

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ParseSingleArgTests () =
    let result, _ = parseFunc([|"def func(arg1: string) -> float: ..."|], 0)
    Assert.AreEqual(
        {
            FunctionDef.Name = "func";
            Type = SimpleType "float"
            Args = [{ Argument.Name = "arg1"; Type = SimpleType "string" }]
        }, result)
    
[<Test>]
let ParseMultiLineArgTests () =
    let source = """def func(arg1: string,
         arg2: int,
         arg3: string) -> float: ...
"""
    
    let result, _ = parseFunc(source.Split("\n"), 0)
    Assert.AreEqual(
        {
            FunctionDef.Name = "func";
            Type = SimpleType "float"
            Args = [{ Argument.Name = "arg1"; Type = SimpleType "string" };
                    { Argument.Name = "arg2"; Type = SimpleType "int" }
                    { Argument.Name = "arg3"; Type = SimpleType "string" } ]
        }, result)

[<Test>]
let ParseManyArgsTests () =
    let result, _ = parseFunc([|"def func ( arg1: string , arg2: int ) -> float: ..."|], 0)
    Assert.AreEqual(
        {
            FunctionDef.Name = "func";
            Type = SimpleType "float"
            Args = [{ Argument.Name = "arg1"; Type = SimpleType "string" };
                    { Argument.Name = "arg2"; Type = SimpleType "int" }]
        }, result)


[<Test>]
let ParseArgWithEnumTests () =
    let result, _ = parseFunc([|"def func ( arg1: string | int, arg2: int ) -> float : ..."|] , 0)
    Assert.AreEqual(
        {
            FunctionDef.Name = "func";
            Type = SimpleType "float"
            Args = [{ Argument.Name = "arg1"; Type = OrType [ SimpleType "string"; SimpleType "int" ] };
                    { Argument.Name = "arg2"; Type = SimpleType "int" }]
        }, result)
    
[<Test>]
let ParseArgWithTupleTests () =
    let result, _ = parseFunc([|"def func ( arg1: tuple[int, string], arg2: int ) -> float : ..."|], 0)
    Assert.AreEqual(
        {
            FunctionDef.Name = "func";
            Type = SimpleType "float"
            Args = [{ Argument.Name = "arg1"; Type = CompositionType ("tuple", [ SimpleType "int"; SimpleType "string" ])};
                    { Argument.Name = "arg2"; Type = SimpleType "int" }]
        }, result)

[<Test>]
let ParseClass () =
    let source = "class MyClass:
    def __init__(self, arg: int) -> None: ...
    def next_method(self, arg1: string, arg2: string) -> None: ...
"
    let result, _ = parseClass(source.Split("\n"), 0, 0)

    Assert.AreEqual(
        {            
            ClassDef.Name = "MyClass";
            Inherits = []
            Items = 
            [
                ClassItem.FunctionDef {
                    FunctionDef.Name = "__init__";
                    Type = SimpleType "None"
                    Args = [{ Argument.Name = "self"; Type = SimpleType ""} ;
                            { Argument.Name = "arg"; Type = SimpleType "int" }]
                };
                ClassItem.FunctionDef {
                    FunctionDef.Name = "next_method";
                    Type = SimpleType "None"
                    Args = [{ Argument.Name = "self"; Type = SimpleType "" };
                            { Argument.Name = "arg1"; Type = SimpleType "string"};
                            { Argument.Name = "arg2"; Type = SimpleType "string" }]
                }
            ]
        }, result)
    
[<Test>]
let ParseClassWithInherits () =
    let source ="""class MyClass(Basic):
    def __init__(self, arg: int) -> None: ...

    def next_method(self, arg1: string, arg2: string) -> int: ...

"""
    let result, _ = parseClass(source.Split("\n"), 0, 0)

    Assert.AreEqual(
        {            
            ClassDef.Name = "MyClass";
            Inherits = [ "Basic" ]
            Items = 
            [
                ClassItem.FunctionDef {
                    FunctionDef.Name = "__init__";
                    Type = SimpleType "None"
                    Args = [ { Argument.Name = "self"; Type = SimpleType "" };
                             { Argument.Name = "arg"; Type = SimpleType "int" }]
                };
                ClassItem.FunctionDef {
                    FunctionDef.Name = "next_method";
                    Type = SimpleType "int"
                    Args = [
                         { Argument.Name = "self"; Type = SimpleType "" };
                         { Argument.Name = "arg1"; Type = SimpleType "string" };
                         { Argument.Name = "arg2"; Type = SimpleType "string" }]
                }
            ]
        }, result)
    
    
[<Test>]
let ParseModule () =
    let source ="""
def func(arg1: string, arg2: int) -> float: ...

class MyClass(Basic):
    def __init__(self, arg: int) -> None: ...

    def next_method(self, arg1: string, arg2: string) -> int: ...

MyVar: tuple[int, string]

"""
    let result = parseModule(source)

    Assert.AreEqual(
        {
            Module.Items = [
                ModuleItem.FunctionDef {
                    FunctionDef.Name = "func";
                    Type = SimpleType "float"
                    Args = [{ Argument.Name = "arg1"; Type = SimpleType "string" };
                            { Argument.Name = "arg2"; Type = SimpleType "int"}]
                };
                ModuleItem.ClassDef {            
                    ClassDef.Name = "MyClass";
                    Inherits = [ "Basic" ]
                    Items = 
                    [
                        ClassItem.FunctionDef {
                            FunctionDef.Name = "__init__";
                            Type = SimpleType "None"
                            Args = [{ Argument.Name = "self"; Type = SimpleType "" };
                                    { Argument.Name = "arg"; Type = SimpleType "int"}]
                        };
                        ClassItem.FunctionDef {
                            FunctionDef.Name = "next_method";
                            Type = SimpleType "int"
                            Args = [{ Argument.Name = "self"; Type = SimpleType "" };
                                    { Argument.Name = "arg1"; Type = SimpleType "string" };
                                    { Argument.Name = "arg2"; Type = SimpleType "string" }]
                        }
                    ]
                }
                ModuleItem.VariableDef {
                    VariableDef.Name = "MyVar"
                    Type = CompositionType ("tuple", [ SimpleType "int"; SimpleType "string" ])
                }
            ]
        }
        , result)


[<Test>]
let ParseModuleWithIFElse () =
    let source ="""

if condition:
    def func1(arg1: string, arg2: int) -> float: ...

else:
    def func2(arg3: string, arg4: int) -> float: ...
"""
    let result = parseModule(source)

    Assert.AreEqual(
        {
            Module.Items = [
                ModuleItem.IfDef {
                    IfDef.Condition = "condition"
                    ThenItems = [
                         ModuleItem.FunctionDef {
                             FunctionDef.Name = "func1";
                             Type = SimpleType "float";
                             Args = [{ Argument.Name = "arg1"; Type = SimpleType "string" };
                                     { Argument.Name = "arg2"; Type = SimpleType "int"}]
                         }
                    ]
                    ElseItems = [
                        ModuleItem.FunctionDef {
                            FunctionDef.Name = "func2";
                            Type = SimpleType "float"
                            Args = [{ Argument.Name = "arg3"; Type = SimpleType "string" };
                                    { Argument.Name = "arg4"; Type = SimpleType "int"}]
                        }
                    ]
                };
            ]
        }
        , result)

[<Test>]
let ParseModuleWithIF () =
    let source ="""

if condition:
    def func1(arg1: string, arg2: int) -> float: ...

def func2(arg3: string, arg4: int) -> float: ...
"""
    let result = parseModule(source)

    Assert.AreEqual(
        {
            Module.Items = [
                ModuleItem.IfDef {
                    IfDef.Condition = "condition"
                    ThenItems = [
                         ModuleItem.FunctionDef {
                             FunctionDef.Name = "func1";
                             Type = SimpleType "float";
                             Args = [{ Argument.Name = "arg1"; Type = SimpleType "string" };
                                     { Argument.Name = "arg2"; Type = SimpleType "int"}]
                         }
                    ]
                    ElseItems = []
                };

                ModuleItem.FunctionDef {
                        FunctionDef.Name = "func2";
                        Type = SimpleType "float"
                        Args = [{ Argument.Name = "arg3"; Type = SimpleType "string" };
                                { Argument.Name = "arg4"; Type = SimpleType "int"}]
                    }
            ]
        }
        , result)
