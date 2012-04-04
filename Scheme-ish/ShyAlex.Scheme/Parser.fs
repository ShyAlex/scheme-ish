module Parser

open System
open System.Text
open Lexer

type expression =
    | Expression of expression list
    | Token of string

let rec parse input =
    let expressionBuffer = ref []
    let completed = ref false
    let tokensToGo = ref input
    while not <| !completed && !tokensToGo <> [] do
        let t = (!tokensToGo).Head
        tokensToGo := (!tokensToGo).Tail
        match t with
        | ExpressionStart -> let (newTokensToGo, exprs) = parse !tokensToGo
                             tokensToGo := newTokensToGo
                             expressionBuffer := Expression(exprs) :: !expressionBuffer
        | ExpressionEnd -> completed := true
        | Lexer.Token(s) -> expressionBuffer := Token(s) :: !expressionBuffer
        | OpenStr(s) -> failwith "Attempted to parse an open string!"
    !tokensToGo, !expressionBuffer |> List.rev