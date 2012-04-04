module Parser2

open System
open Keyword
open Literal
open Env

type expression =
    | Scope of expression env * expression
    | Literal of literal
    | Variable of string
    | Keyword of keyword
    | Expression of expression list
    override this.ToString() =
        match this with
        | Scope(_, e) -> e.ToString()
        | Literal(l) -> l.ToString()
        | Variable(s) -> s
        | Keyword(kw) -> kw.ToString()
        | Expression(exprs) -> "(" + (exprs |> List.map (sprintf "%O") |> List.fold (fun s e -> s + " " + e) "") + " )"

let rec printExprWithScope = function
    | Scope(_, e) -> "[[ " + (printExprWithScope e) + " ]]"
    | Literal(l) -> l.ToString()
    | Variable(s) -> s
    | Keyword(kw) -> kw.ToString()
    | Expression(exprs) -> "(" + (exprs |> List.map printExprWithScope |> List.fold (fun s e -> s + " " + e) "") + " )"
    
let toDouble = function Literal(Number(n)) -> n | _ -> failwith "expected number"

let toBoolean = function Literal(Boolean(b)) -> b | _ -> failwith "expected boolean"

let rec parse = function
    | Parser.Token(s) -> match keyword.FromString s with
                         | Some(kw) -> Keyword(kw)
                         | None -> match literal.FromString(s) with 
                                   | Some(l) -> Literal(l)
                                   | None -> Variable(s)
    | Parser.Expression(es) -> Expression(es |> List.map parse)