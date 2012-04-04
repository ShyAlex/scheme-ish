module Lexer

open System
open System.Text

type token =
    | ExpressionStart
    | ExpressionEnd
    | OpenStr of string
    | Token of string

let private startExpressionChar = '('
let private endExpressionChar = ')'
let private stringDelimChar = '"'
let private stringEscapeChar = '\\'

let lex input =
    [ let tokenBuffer = StringBuilder()
      let isInString = ref false
      let isCharEscaped = ref false
      for c in input do
          if (not <| !isInString) && 
             (c = startExpressionChar || c = endExpressionChar || Char.IsWhiteSpace(c)) && 
             (tokenBuffer.Length > 0) then
              yield Token(tokenBuffer.ToString())
              tokenBuffer.Length <- 0
          if not <| !isInString then
              if c = startExpressionChar then yield ExpressionStart
              elif c = endExpressionChar then yield ExpressionEnd
              elif not <| Char.IsWhiteSpace(c) then 
                  tokenBuffer.Append(c) |> ignore
                  isInString := c = stringDelimChar
          else
              if !isCharEscaped then
                  if c <> stringEscapeChar && c <> stringDelimChar then
                      tokenBuffer.Append(stringEscapeChar) |> ignore
                  tokenBuffer.Append(c) |> ignore
                  isCharEscaped := false
              elif c = stringEscapeChar then
                  isCharEscaped := true
              elif c = stringDelimChar then
                  tokenBuffer.Append(c) |> ignore
                  isInString := false
              else
                  tokenBuffer.Append(c) |> ignore
      if tokenBuffer.Length > 0 then
          if !isInString then yield OpenStr(tokenBuffer.ToString())
                         else yield Token(tokenBuffer.ToString()) ]

type lexValidation =
    | Valid
    | Incomplete
    | OverClosed
    | OpenString

let validateLex input =
    let rec validateLex expStarts expEnds input =
        if expStarts < expEnds then OverClosed else
        match input with
        | [] -> if expStarts = expEnds then Valid else Incomplete
        | h :: t -> match h with
                    | ExpressionStart -> validateLex (expStarts + 1) expEnds t
                    | ExpressionEnd -> validateLex expStarts (expEnds + 1) t
                    | OpenStr(_) -> OpenString
                    | _ -> validateLex expStarts expEnds t
    validateLex 0 0 input