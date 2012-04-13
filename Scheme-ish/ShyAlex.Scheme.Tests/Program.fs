module Tests

open System
open System.IO
open System.Threading
open Types
open Lexer
open Parser2
open Interpreter
open NUnit.Framework

type result =
    | Fail of string
    | Result of expression
    | Ex of Exception
    | Timeout
    override this.ToString() =
        match this with
        | Fail(s) -> sprintf "Fail(%s)" s
        | Result(e) -> sprintf "Result(%O)" e
        | Ex(ex) -> sprintf "Ex(%O)" ex
        | Timeout -> "Timeout"

let private testTimeoutMs = 1000.0

let private executeTest expectedResult file =
    let input = File.ReadAllText file
    let env = ref(Env.newEnv())
    let testResult = ref (Fail("test did not update result"))
    let endTime = DateTime.Now.AddMilliseconds(testTimeoutMs)
    try
        let tokens = input |> lex
        match tokens |> validateLex with
        | Incomplete -> testResult := Fail("Statement was not complete")
        | OverClosed -> testResult := Fail("Statement was over-closed")
        | OpenString -> testResult := Fail("Statement was a non-terminated string")
        | Valid -> let (chars, output) = tokens |> Parser.parse
                   let program = output |> List.map parse
                   let result = ref(Literal(Nil))
                   let allStatements = program |> Seq.map (fun expr -> reduce !env expr) |> Seq.concat
                   use e = allStatements.GetEnumerator()
                   let bail = ref false
                   while not <| !bail && e.MoveNext() do
                       let (newResult, newEnv) = e.Current
                       Console.WriteLine(newResult |> printExprWithScope)
                       result := newResult
                       env := newEnv
                       if DateTime.Now > endTime then 
                           bail := true
                           testResult := Timeout
                   match !testResult, chars with
                   | Timeout, _ -> ()
                   | _, [] -> testResult := Result(!result)
                   | _, _ -> testResult := Fail(sprintf "Some characters did not get parsed: %A" chars)
    with ex -> testResult := Ex(ex)
    match !testResult with
    | Fail(s) -> Assert.Fail(s)
    | Ex(ex) -> Assert.Fail(sprintf "Program threw an exception:\r\ncommand:%s\r\nenv: %O\r\nexpected: %A\r\nactual: %A" input !env expectedResult ex)
    | result -> Assert.AreEqual(expectedResult, result, sprintf "The executed result was not as expected:\r\ncommand: %s\r\nactual: %O\r\nexpected: %O" input result expectedResult)
    
[<Test>]
let literalTen () = "LiteralTen.ss" |> executeTest (Result(Literal(Number(10.0))))

[<Test>]
let literalExpressionSeven () = "LiteralExpressionSeven.ss" |> executeTest (Result(Literal(Number(7.0))))
        
[<Test>]
let simpleAddition () = "SimpleAddition.ss" |> executeTest (Result(Literal(Number(12.0))))
        
[<Test>]
let simpleSubtraction () = "SimpleSubtraction.ss" |> executeTest (Result(Literal(Number(8.0))))

[<Test>]
let simpleDivision () = "SimpleDivision.ss" |> executeTest (Result(Literal(Number(3.0))))

[<Test>]
let compundMaths () = "CompoundMaths.ss" |> executeTest (Result(Literal(Number(6.0))))

[<Test>]
let simpleDefine () = "SimpleDefine.ss" |> executeTest (Result(Literal(Nil)))

[<Test>]
let chainedDefine () = "ChainedDefine.ss" |> executeTest (Result(Literal(Nil)))

[<Test>]
let mathsWithVariables () = "MathsWithVariables.ss" |> executeTest (Result(Literal(Number(19.0))))

[<Test>]
let equalityWithVariables () = "EqualityWithVariables.ss" |> executeTest (Result(Literal(Boolean(false))))

[<Test>]
let mathsAndComparisonsWithVariables () = "MathsAndComparisonsWithVariables.ss" |> executeTest (Result(Literal(Number(4.0))))

[<Test>]
let condWithVariables () = "CondWithVariables.ss" |> executeTest (Result(Literal(Number(16.0))))

[<Test>]
let ifWithVariables () = "IfWithVariables.ss" |> executeTest (Result(Literal(Number(6.0))))

[<Test>]
let complexCondWithVariables () = "ComplexCondWithVariables.ss" |> executeTest (Result(Literal(Number(16.0))))

[<Test>]
let sicpMathsTest () = "SicpMathsTest.ss" |> executeTest (Result(Literal(Number(37.0 / -150.0))))

[<Test>]
let sumsq1 () = "SumSq1.ss" |> executeTest (Result(Literal(Number(41.0))))

[<Test>]
let sumsq2 () = "SumSq2.ss" |> executeTest (Result(Literal(Number(41.0))))

[<Test>]
let sumsq3 () = "SumSq3.ss" |> executeTest (Result(Literal(Number(41.0))))

[<Test>]
let aPlusAbsB () = "APlusAbsB.ss" |> executeTest (Result(Literal(Number(9.0))))

[<Test>]
let infiniteLoop () = "InfiniteLoop.ss" |> executeTest Timeout

[<Test>]
let newIf () = "NewIf.ss" |> executeTest (Result(Literal(Nil)))

[<Test>]
let newtonSqrt () = "NewtonSqrt.ss" |> executeTest (Result(Literal(Number(3.00009155413138))))

[<Test>]
let procAsVariable () = "ProcAsVariable.ss" |> executeTest (Result(Literal(Number(8.0))))

[<Test>]
let compundAnd () = "CompoundAnd.ss" |> executeTest (Result(Literal(Boolean(false))))

[<Test>]
let squareTest () = "SquareTest.ss" |> executeTest (Result(Literal(Number(49.0))))

[<Test>]
let simpleCompoundMaths () = "SimpleCompoundMaths.ss" |> executeTest (Result(Literal(Number(13.0))))

[<Test>]
let areaCircle () = "AreaCircle.ss" |> executeTest (Result(Literal(Number(78.5))))

[<Test>]
let recursiveFac () = "RecursiveFac.ss" |> executeTest (Result(Literal(Number(120.0))))

[<Test>]
let sumsqWithInnerProc () = "SumSqWithInnerProc.ss" |> executeTest (Result(Literal(Number(25.0))))

[<Test>]
let lessThan () = "LessThan.ss" |> executeTest (Result(Literal(Boolean(true))))

[<Test>]
let sumCubes () = "SumCubes.ss" |> executeTest (Result(Literal(Number(3025.0))))

[<Test>]
let sumIntegers () = "SumIntegers.ss" |> executeTest (Result(Literal(Number(55.0))))

[<Test>]
let ackermann () = "Ackermann.ss" |> executeTest (Result(Literal(Number(65536.0))))

[<Test>]
let fibIter () = "FibIter.ss" |> executeTest (Result(Literal(Number(21.0))))

[<Test>]
let lambda () = "Lambda.ss" |> executeTest (Result(Literal(Number(11.0))))

[<Test>]
let lambdaAsArg () = "LambdaAsArg.ss" |> executeTest (Result(Literal(Number(27.0))))

[<Test>]
let piSum () = "PiSum.ss" |> executeTest (Result(Literal(Number(3.1031453128860114))))

[<Test>]
let simpleLet () = "SimpleLet.ss" |> executeTest (Result(Literal(Number(3.0))))

[<Test>]
let complexLet () = "ComplexLet.ss" |> executeTest (Result(Literal(Number(4.0))))

[<Test>]
let letValueEvaluation1 () = "LetValueEvaluation1.ss" |> executeTest (Result(Literal(Number(38.0))))

[<Test>]
let letValueEvaluation2 () = "LetValueEvaluation2.ss" |> executeTest (Result(Literal(Number(12.0))))

[<Test>]
let letDefineLambdaCombo () = "LetDefineLambdaCombo.ss" |> executeTest (Result(Literal(Number(45.0))))

[<Test>]
let lambdaRepresentationOfCons () = "LambdaRepresentationOfCons.ss" |> executeTest (Result(Literal(Number(1.0))))

[<Test>]
let procRepresentationOfCons () = "ProcRepresentationOfCons.ss" |> executeTest (Result(Literal(Number(1.0))))

[<Test>]
let overwriteClosedOverVars () = "OverwriteClosedOverVars.ss" |> executeTest (Result(Literal(Number(1.0))))

[<Test>]
let shortcutAnd () = "ShortcutAnd.ss" |> executeTest (Result(Literal(Boolean(false))))

[<Test>]
let shortcutOr () = "ShortcutOr.ss" |> executeTest (Result(Literal(Boolean(true))))

[<Test>]
let redefineKeyword () = "RedefineKeyword.ss" |> executeTest (Result(Literal(Number(-1.0))))

[<Test>]
let display () = "Display.ss" |> executeTest (Result(Literal(Nil)))

[<Test>]
let newline () = "Newline.ss" |> executeTest (Result(Literal(Nil)))

[<Test>]
let facIterYComb () = "FacIterYComb.ss" |> executeTest (Result(Literal(Number(120.0))))

[<Test>]
let errorUnknownVariable () = "ErrorUnknownVariable.ss" |> executeTest (Result(Error("unknown variable: f")))

[<Test>]
let errorUnknownVariableExpression () = "ErrorUnknownVariableExpression.ss" |> executeTest (Result(Error("unknown variable: f")))

[<Test>]
let errorUnknownVariableExpressionExpression () = "ErrorUnknownVariableExpressionExpression.ss" |> executeTest (Result(Error("unknown variable: f")))

[<Test>]
let errorExpectedNumber () = "ErrorExpectedNumber.ss" |> executeTest (Result(Error("expected number")))

[<Test>]
let errorExpectedTwoNumbers () = "ErrorExpectedTwoNumbers.ss" |> executeTest (Result(Error("expected two numbers")))