module ExpressionEnv

open Env
open Keyword
open Literal
open Parser2

let private add (env:expression env) = function
    | Variable(funcName), exprs -> env.SetVar funcName ([], [exprs])
    | Expression(Variable(funcName) :: funcArgs), exprs -> env.SetVar funcName (funcArgs, [exprs])
    | _ -> failwith "expected a variable or expression containing a variable"

let addAll keys values (env:expression env) =
    List.zip keys values |> List.fold (fun s (k, v) -> add s (k, v)) env

let setVar x y (env:'a env) =
    env.SetVar x y

let newEnv () =
    env<expression>()
    |> setVar "+" ([], [ Keyword(Add) ])
    |> setVar "-" ([], [ Keyword(Subtract) ])
    |> setVar "*" ([], [ Keyword(Multiply) ])
    |> setVar "/" ([], [ Keyword(Divide) ])
    |> setVar "=" ([], [ Keyword(Equal) ])
    |> setVar "<" ([], [ Keyword(LessThan) ])
    |> setVar ">" ([], [ Keyword(GreaterThan) ])
    |> setVar "<=" ([], [ Keyword(LessThanOrEqual) ])
    |> setVar ">=" ([], [ Keyword(GreaterThanOrEqual) ])
    |> setVar "not" ([ Variable("arg") ], [ Expression(Keyword(If) :: Expression(Keyword(Equal) :: Variable("arg") :: Literal(Boolean(false)) :: []) :: Literal(Boolean(true)) :: Literal(Boolean(false)) :: []) ])
    |> setVar "display" ([], [ Keyword(Display) ])
    |> setVar "newline" ([], [ Keyword(Newline) ])