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
