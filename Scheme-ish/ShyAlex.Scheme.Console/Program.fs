open System
open System.Collections.Generic
open System.Text
open Types
open Lexer
open Parser2
open Interpreter

let private normalColor = Console.ForegroundColor
let private promptColor = ConsoleColor.Cyan
let private errorColor = ConsoleColor.Red
let private responseColor = ConsoleColor.Green
let private traceColor = ConsoleColor.Magenta
let private envTraceColor = ConsoleColor.Yellow

let private write c (s:string) =
    Console.ForegroundColor <- c
    Console.Write(s)
    Console.ForegroundColor <- normalColor

let private writen c (s:string) = write c (s + Environment.NewLine)

let private run exprToStr =
    let env = ref(Env.newEnv())
    let buffer = StringBuilder()
    while true do
        write promptColor (if buffer.Length < 1 then ">> " else ".  ")
        buffer.AppendLine(Console.ReadLine()) |> ignore
        let tokens = buffer.ToString() |> lex
        match tokens |> validateLex with
        | Valid -> buffer.Length <- 0
                   let lastResult = ref(Literal(Nil))
                   let (_, firstPassOutput) = tokens |> Parser.parse
                   let program = firstPassOutput |> List.map parse
                   try
                       for expr in program do reduce !env expr |> Seq.iter (fun (newResult, newEnv) -> newResult |> exprToStr |> writen traceColor
                                                                                                       lastResult := newResult
                                                                                                       env := newEnv)
                       (!env).ToString() |> write envTraceColor
                       let output, color = match !lastResult with
                                           | Keyword(Exit) -> exit 0
                                           | Error(e) -> e, errorColor
                                           | Literal(l) -> l.ToString(), responseColor
                                           | _ -> "unknown error, result was " + ((!lastResult).ToString()), errorColor
                       if output <> null then writen color output
                   with ex -> writen errorColor ex.Message
        | Incomplete -> ()
        | OpenString -> ()
        | OverClosed -> buffer.Length <- 0
                        writen errorColor "expression was over-closed - try removing some closing parentheses"

run (if Environment.GetCommandLineArgs() |> Array.exists (fun arg -> arg = "noscope") then (sprintf "%O") else printExprWithScope)