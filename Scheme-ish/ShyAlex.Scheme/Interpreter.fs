module Interpreter

open System
open System.Collections.Generic
open Keyword
open Literal
open Parser2
open Env
open ExpressionEnv

let rec private reduceMath op this = function
    | [] -> this
    | var :: tail -> reduceMath op (Literal(Number(op (toDouble this) (toDouble var)))) tail
    
let private reduceComparison op = function
    | expr1 :: expr2 :: [] -> op (toDouble expr1) (toDouble expr2) |> Boolean |> Literal
    | _ -> failwith "comparison operator has incorrect arguments"

let rec private reduceCond env reduce = function
    | [] -> failwith "no condition evaluated to #t"
    | cond :: tail -> match cond with
                      | Expression(Keyword(Else) :: expr :: []) -> expr
                      | Expression(Literal(Boolean(true)) :: expr :: []) -> expr
                      | Expression(Literal(Boolean(false)) :: expr :: []) -> Expression(Keyword(Cond) :: tail)
                      | Expression(expr :: []) -> expr
                      | Expression(condExpr :: expr) -> Expression(Keyword(Cond) :: Expression(reduce env condExpr :: expr) :: tail) 
                      | _ -> failwith "cond not as expected"

let private reduceIf env reduce = function
    | condExpr :: trueExpr :: falseExpr :: [] -> match condExpr with
                                                 | Literal(Boolean(b)) -> if b then trueExpr else falseExpr
                                                 | _ -> Expression(Keyword(If) :: reduce env condExpr :: trueExpr :: falseExpr :: [])
    | _ -> failwith "if not as expected"

let private reduceEqual = function
    | expr1 :: expr2 :: [] -> expr1.Equals(expr2) |> Boolean |> Literal
    | _ -> failwith "= not as expected"

let rec reduceAnd stepReduce = function
    | [] -> Literal(Boolean(true))
    | Literal(Boolean(false)) :: t -> Literal(Boolean(false))
    | Literal(Boolean(true)) :: t -> Expression(Keyword(And) :: t)
    | expr :: t -> Expression(Keyword(And) :: stepReduce expr :: t)

let rec reduceOr stepReduce = function
    | [] -> Literal(Boolean(false))
    | Literal(Boolean(true)) :: t -> Literal(Boolean(true))
    | Literal(Boolean(false)) :: t -> Expression(Keyword(Or) :: t)
    | expr :: t -> Expression(Keyword(Or) :: stepReduce expr :: t)

let private reduceDefine (env:expression env) = function
    | Variable(x) :: varExprs -> env.SetVar x ([], varExprs)
    | Expression(Variable(x) :: funcArgs) :: funcExprs -> env.SetVar x (funcArgs, funcExprs)
    | _ -> failwith "define not as expected"

let rec private reduceLet (env:expression env) (args:expression list) (body:expression list) =
    let signature = args |> List.map (function Expression(Variable(v) :: expr :: []) -> Variable(v) | _ -> failwith "let not as expected")
    let lambdaArgs = args |> List.map (function Expression(_ :: expr :: []) -> expr | _ -> failwith "let not as expected")
    Expression(Expression(Keyword(Lambda) :: Expression(signature) :: body) :: lambdaArgs)

let reduceNewline = function
    | [] -> printfn ""
            Literal(Nil)
    | args -> failwith (sprintf "Invalid argument count: %i" args.Length)

let reduceDisplay = function
    | arg :: [] -> printf "%O" arg
                   Literal(Nil)
    | args -> failwith (sprintf "Invalid argument count: %i" args.Length)

let rec private isLiteral (env:expression env) = function
    | Literal(_) -> true
    | Keyword(_) -> true
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

let rec private collapseScopes isTopLevel = function
    | Scope(env, Scope(env', expr)) when not isTopLevel -> Scope(env.Combine(env'), expr) |> collapseScopes false
    | Scope(env, Scope(env', expr)) -> Scope(env, Scope(env', expr) |> collapseScopes false)
    | Scope(env, expr) -> Scope(env, collapseScopes false expr)
    | Expression(exprs) -> Expression(exprs |> List.map (collapseScopes false))
    | expr -> expr

let rec private stepReduce (env:expression env) = function
    | Scope(env', expr) -> match expr with
                           | Literal(l) -> Literal(l)
                           | Keyword(k) -> match k with
                                           | Newline -> reduceNewline []
                                           | _ -> Keyword(k)
                           | Expression(Keyword(Define) :: args) -> match args with
                                                                    | Variable(varName) :: statements -> match resolve1 stepReduce env' statements with
                                                                                                         | (true, newStatements) -> Scope(env', Expression(Keyword(Define) :: Variable(varName) :: newStatements))
                                                                                                         | (false, _) -> let newEnv = reduceDefine env args
                                                                                                                         Scope(newEnv, Literal(Nil))
                                                                    | _ -> let newEnv = reduceDefine env args
                                                                           Scope(newEnv, Literal(Nil))
                           | Scope(env'', expr) -> Scope(env', Scope(env'', expr) |> stepReduce env'')
                           | _ -> Scope(env', stepReduce env' expr)
    | Variable(var) -> match env.GetVar var with
                       | Some([], funcExpr :: []) -> funcExpr
                       | Some([], funcExprs) -> Expression(funcExprs)
                       | Some(funcArgs, funcExprs) -> Expression(Keyword(Lambda) :: Expression(funcArgs) :: funcExprs)
                       | None -> failwith <| "unrecognized variable: " + var
    | Expression(exprs) -> match exprs with
                           | Expression(Keyword(Define) :: args) :: tail -> match args with
                                                                            | Variable(varName) :: statements -> match resolve1 stepReduce env statements with
                                                                                                                 | (true, newStatements) -> Expression(Expression(Keyword(Define) :: Variable(varName) :: newStatements) :: tail)
                                                                                                                 | (false, _) -> let newEnv = reduceDefine env args
                                                                                                                                 Scope(newEnv, Expression(Literal(Nil) :: tail))
                                                                            | _ -> let newEnv = reduceDefine env args
                                                                                   Scope(newEnv, Expression(Literal(Nil) :: tail))
                           | Expression(Keyword(Lambda) :: Expression(signature) :: exprs) :: args -> match resolve1 stepReduce env args with
                                                                                                      | (true, newArgs) -> Expression(Expression(Keyword(Lambda) :: Expression(signature) :: exprs) :: newArgs)
                                                                                                      | (false, _) -> match args with
                                                                                                                      | [] -> Expression(Keyword(Lambda) :: Expression(signature) :: exprs)
                                                                                                                      | _ -> let newEnv = env |> ExpressionEnv.addAll signature args
                                                                                                                             Scope(newEnv, Expression(exprs))
                           | Scope(env', Expression(Keyword(Lambda) :: Expression(signature) :: exprs)) :: args -> match resolve1 stepReduce env' args with
                                                                                                                   | (true, newArgs) -> Expression(Scope(env', Expression(Keyword(Lambda) :: Expression(signature) :: exprs)) :: newArgs)
                                                                                                                   | (false, _) -> match args with
                                                                                                                                   | [] -> Scope(env', Expression(Keyword(Lambda) :: Expression(signature) :: exprs))
                                                                                                                                   | _ -> let newEnv = env' |> ExpressionEnv.addAll signature args
                                                                                                                                          Scope(newEnv, Expression(exprs))
                           | Keyword(Let) :: Expression(args) :: body -> reduceLet env args body
                           | Keyword(If) :: args -> reduceIf env stepReduce args
                           | Keyword(Cond) :: args -> reduceCond env stepReduce args
                           | Keyword(And) :: args -> reduceAnd (stepReduce env) args
                           | Keyword(Or) :: args -> reduceOr (stepReduce env) args
                           | Literal(Nil) :: [] -> Literal(Nil)
                           | Literal(Nil) :: arg :: [] -> arg
                           | Literal(Nil) :: args -> Expression(args)
                           | Variable(funcName) :: args -> match resolve1 stepReduce env args with
                                                           | (true, newArgs) -> Expression(Variable(funcName) :: newArgs)
                                                           | _ -> match env.GetVar funcName with
                                                                  | Some(funcArgs, funcExprs) -> if args.Length = 0 && funcArgs.Length <> 0 then Variable(funcName)
                                                                                                 elif funcArgs.Length = 0 && args.Length <> 0 then Expression(List.append funcExprs args)
                                                                                                 else let newEnv = env |> ExpressionEnv.addAll funcArgs args                  
                                                                                                      match funcExprs with
                                                                                                      | funcExpr :: [] -> Scope(newEnv, funcExpr)
                                                                                                      | _ -> Scope(newEnv, Expression(funcExprs))
                                                                  | None -> failwith <| "unrecognised variable: " + funcName
                           | _ -> match resolve1 stepReduce env exprs with
                                  | (true, newExprs) -> Expression(newExprs)
                                  | _ -> match exprs with
                                         | Keyword(kw) :: [] -> match kw with
                                                                | Newline -> reduceNewline []
                                                                | _ -> Keyword(kw)
                                         | Keyword(kw) :: h :: t -> match kw with
                                                                    | Add -> reduceMath (+) h t
                                                                    | Subtract -> reduceMath (-) h t
                                                                    | Divide -> reduceMath (/) h t
                                                                    | Multiply -> reduceMath (*) h t
                                                                    | LessThan -> reduceComparison (<) (h :: t)
                                                                    | LessThanOrEqual -> reduceComparison (<=) (h :: t)
                                                                    | GreaterThan -> reduceComparison (>) (h :: t)
                                                                    | GreaterThanOrEqual -> reduceComparison (>=) (h :: t)
                                                                    | Equal -> reduceEqual (h :: t)
                                                                    | Display -> reduceDisplay (h :: t)
                                                                    | _ -> failwith "unexpected keyword"
                                         | l :: [] when isLiteral env l -> l
                                         | [] -> Literal(Nil)
                                         | _ -> failwith "unable to reduce expression"
    | literal -> literal

let reduce env expr = 
    let rec reduce env expr =
        seq { let expr = expr |> collapseScopes true
              let topLevelEnv = ref env
              match expr with
              | Literal(l) -> yield expr, !topLevelEnv
              | Keyword(Exit) -> yield expr, !topLevelEnv
              | Scope(env', _) -> topLevelEnv := env'
                                  yield expr, !topLevelEnv
                                  yield! stepReduce !topLevelEnv expr |> reduce !topLevelEnv
              | expr -> yield expr, !topLevelEnv
                        yield! stepReduce !topLevelEnv expr |> reduce !topLevelEnv }
    Scope(env, expr) |> reduce env