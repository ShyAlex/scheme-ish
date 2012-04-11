module Env

open Types

let private add (env:env) = function
    | Variable(funcName), exprs -> env.SetVar funcName ([], [exprs])
    | Expression(Variable(funcName) :: funcArgs), exprs -> env.SetVar funcName (funcArgs, [exprs])
    | _ -> failwith "expected a variable or expression containing a variable"

let addAll keys values (env:env) =
    List.zip keys values |> List.fold (fun s (k, v) -> add s (k, v)) env

let setVar x y (env:env) =
    env.SetVar x y

let newEnv () =
    env()
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