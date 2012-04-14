module Interpreter

open System
open System.Collections.Generic
open Types

let rec private reduceMath kw op = function
    | Literal(Number(n)) :: [] -> Literal(Number(n))
    | Literal(Number(n)) :: Literal(Number(n')) :: tail -> Expression(kw :: (Literal(Number(op n n')) :: tail))
    | _ -> Error("expected number")
    
let private reduceComparison op = function
    | Literal(Number(n)) :: Literal(Number(n')) :: [] -> Literal(Boolean(op n n'))
    | _ -> Error("expected two numbers")

let rec private reduceCond env reduce = function
    | [] -> Error("no condition evaluated to #t")
    | cond :: tail -> match cond with
                      | Expression(Keyword(Else) :: expr :: []) -> expr
                      | Expression(Literal(Boolean(true)) :: expr :: []) -> expr
                      | Expression(Literal(Boolean(false)) :: expr :: []) -> Expression(Keyword(Cond) :: tail)
                      | Expression(expr :: []) -> expr
                      | Expression(condExpr :: expr) -> Expression(Keyword(Cond) :: Expression(reduce env condExpr :: expr) :: tail) 
                      | _ -> Error("cond not as expected")

let private reduceIf env reduce = function
    | condExpr :: trueExpr :: falseExpr :: [] -> match condExpr with
                                                 | Literal(Boolean(b)) -> if b then trueExpr else falseExpr
                                                 | _ -> Expression(Keyword(If) :: reduce env condExpr :: trueExpr :: falseExpr :: [])
    | _ -> Error("if not as expected")

let private reduceEqual = function
    | expr1 :: expr2 :: [] -> expr1.Equals(expr2) |> Boolean |> Literal
    | _ -> Error("= not as expected")

let rec private reduceAnd stepReduce = function
    | [] -> Literal(Boolean(true))
    | Literal(Boolean(false)) :: t -> Literal(Boolean(false))
    | Literal(Boolean(true)) :: t -> Expression(Keyword(And) :: t)
    | expr :: t -> Expression(Keyword(And) :: stepReduce expr :: t)

let rec private reduceOr stepReduce = function
    | [] -> Literal(Boolean(false))
    | Literal(Boolean(true)) :: t -> Literal(Boolean(true))
    | Literal(Boolean(false)) :: t -> Expression(Keyword(Or) :: t)
    | expr :: t -> Expression(Keyword(Or) :: stepReduce expr :: t)

let rec private reduceLet (env:env) (args:expression list) (body:expression list) =
    let signature = args |> List.map (function Expression(Variable(v) :: expr :: []) -> Variable(v) | _ -> Error("let not as expected"))
    let lambdaArgs = args |> List.map (function Expression(_ :: expr :: []) -> expr | _ -> Error("let not as expected"))
    Expression(Expression(Keyword(Lambda) :: Expression(signature) :: body) :: lambdaArgs)

let private reduceNewline = function
    | [] -> printfn ""
            Literal(Nil)
    | args -> Error(sprintf "invalid argument count: %i" args.Length)

let private reduceDisplay = function
    | arg :: [] -> printf "%O" arg
                   Literal(Nil)
    | args -> Error(sprintf "invalid argument count: %i" args.Length)

let rec private isLiteral (env:env) = function
    | Literal(_) -> true
    | Keyword(_) -> true
    | Error(_) -> true
    | Expression(Keyword(Lambda) :: _) -> true
    | Scope(_, Literal(_)) -> false
    | Scope(_, Keyword(_)) -> false
    | Scope(env', expr) -> isLiteral env' expr
    | Variable(funcName) -> match env.GetVar funcName with
                            | Some(_) -> false
                            | _ -> true
    | _ -> false

let private resolve1 stepReduce env args =
    let bail = ref false
    let newArgs = args |> List.map (fun arg -> if !bail then arg elif isLiteral env arg then arg
                                               else bail := true; stepReduce env arg)
    !bail, newArgs

let private reduceDefine stepReduce env tail = function
    | Variable(varName) :: statements -> match resolve1 stepReduce env statements with
                                         | (true, newStatements) -> Expression(Expression(Keyword(Define) :: Variable(varName) :: newStatements) :: tail)
                                         | (false, _) -> let newEnv = env.SetVar varName ([], statements)
                                                         Scope(newEnv, Expression(Literal(Nil) :: tail))
    | Expression(Variable(x) :: funcArgs) :: funcExprs -> let newEnv = env.SetVar x (funcArgs, funcExprs)
                                                          Scope(newEnv, Expression(Literal(Nil) :: tail))
    | _ -> Error("define not as expected")

let private reduceLambda stepReduce env signature exprs args = 
    match resolve1 stepReduce env args with
    | (true, newArgs) -> Expression(Scope(env, Expression(Keyword(Lambda) :: Expression(signature) :: exprs)) :: newArgs)
    | (false, _) -> let newEnv = env |> Env.addAll signature args
                    Scope(newEnv, Expression(exprs))

let private reduceEnvLookup stepReduce env funcName args =
    match resolve1 stepReduce env args with
    | (true, newArgs) -> Expression(Variable(funcName) :: newArgs)
    | _ -> match env.GetVar funcName with
           | Some(funcArgs, funcExprs) -> if args.Length = 0 && funcArgs.Length <> 0 then Variable(funcName)
                                          elif funcArgs.Length = 0 && args.Length <> 0 then Expression(List.append funcExprs args)
                                          else let newEnv = env |> Env.addAll funcArgs args                  
                                               match funcExprs with
                                               | funcExpr :: [] -> Scope(newEnv, funcExpr)
                                               | _ -> Scope(newEnv, Expression(funcExprs))
           | None -> Error("unknown variable: " + funcName)

let private reduceBaseExpression env = function
    | Keyword(kw) :: [] -> match kw with
                            | Newline -> reduceNewline []
                            | _ -> Keyword(kw)
    | Keyword(kw) :: args -> match kw with
                             | Add -> reduceMath (Keyword(kw)) (+) args
                             | Subtract -> reduceMath (Keyword(kw)) (-) args
                             | Divide -> reduceMath (Keyword(kw)) (/) args
                             | Multiply -> reduceMath (Keyword(kw)) (*) args
                             | LessThan -> reduceComparison (<) args
                             | LessThanOrEqual -> reduceComparison (<=) args
                             | GreaterThan -> reduceComparison (>) args
                             | GreaterThanOrEqual -> reduceComparison (>=) args
                             | Equal -> reduceEqual args
                             | Display -> reduceDisplay args
                             | _ -> Error("unexpected keyword")
    | l :: [] when isLiteral env l -> l
    | [] -> Literal(Nil)
    | _ -> Error("unable to reduce expression")

let private reduceScope stepReduce env env' = function
    | Literal(l) -> Literal(l)
    | Error(e) -> Error(e)
    | Keyword(k) -> match k with
                    | Newline -> reduceNewline []
                    | _ -> Keyword(k)
    | Scope(env'', expr) -> Scope(env', Scope(env'', expr) |> stepReduce env'')
    | Expression(Keyword(Define) :: args) -> reduceDefine stepReduce env [] args
    | expr -> Scope(env', stepReduce env' expr)

let private reduceVariable env var = function
    | Some([], funcExpr :: []) -> funcExpr
    | Some([], funcExprs) -> Expression(funcExprs)
    | Some(funcArgs, funcExprs) -> Expression(Keyword(Lambda) :: Expression(funcArgs) :: funcExprs)
    | None -> Error("unknown variable: " + var)

let private reduceExpression stepReduce env = function
    | Expression(Keyword(Define) :: args) :: tail -> reduceDefine stepReduce env tail args
    | Expression(Keyword(Lambda) :: Expression(signature) :: exprs) :: args -> reduceLambda stepReduce env signature exprs args
    | Scope(env', Expression(Keyword(Lambda) :: Expression(signature) :: exprs)) :: args -> reduceLambda stepReduce env' signature exprs args
    | Keyword(Let) :: Expression(args) :: body -> reduceLet env args body
    | Keyword(If) :: args -> reduceIf env stepReduce args
    | Keyword(Cond) :: args -> reduceCond env stepReduce args
    | Keyword(And) :: args -> reduceAnd (stepReduce env) args
    | Keyword(Or) :: args -> reduceOr (stepReduce env) args
    | Literal(Nil) :: [] -> Literal(Nil)
    | Literal(Nil) :: arg :: [] -> arg
    | Literal(Nil) :: args -> Expression(args)
    | Variable(funcName) :: args -> reduceEnvLookup stepReduce env funcName args
    | exprs -> match resolve1 stepReduce env exprs with
               | (true, newExprs) -> Expression(newExprs)
               | _ -> reduceBaseExpression env exprs

let rec private stepReduce (env:env) = function
    | Scope(env', expr) -> reduceScope stepReduce env env' expr
    | Variable(var) -> env.GetVar var |> reduceVariable env var
    | Expression(exprs) -> reduceExpression stepReduce env exprs
    | expr when isLiteral env expr -> expr
    | _ -> Error("unable to reduce expression")

let rec private collapseScopes isTopLevel = function
    | Scope(env, Scope(env', expr)) when not isTopLevel -> Scope(env.Combine(env'), expr) |> collapseScopes false
    | Scope(env, Scope(env', expr)) -> Scope(env, Scope(env', expr) |> collapseScopes false)
    | Scope(env, expr) -> Scope(env, collapseScopes false expr)
    | Expression(exprs) -> Expression(exprs |> List.map (collapseScopes false))
    | expr -> expr

let reduce env expr = 
    let rec reduce env expr =
        seq { let expr = expr |> collapseScopes true
              let topLevelEnv = ref env
              match expr with
              | Scope(env', _) -> topLevelEnv := env'
                                  yield expr, !topLevelEnv
                                  yield! stepReduce !topLevelEnv expr |> reduce !topLevelEnv
              | expr -> yield expr, !topLevelEnv
                        if not <| isLiteral !topLevelEnv expr then yield! stepReduce !topLevelEnv expr |> reduce !topLevelEnv }
    Scope(env, expr) |> reduce env