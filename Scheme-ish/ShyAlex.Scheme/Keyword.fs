module Keyword

type keyword =
    | If
    | Cond
    | Add
    | Subtract
    | Divide
    | Multiply
    | And
    | Or
    | Equal
    | GreaterThan
    | LessThan
    | GreaterThanOrEqual
    | LessThanOrEqual
    | Define
    | Else
    | Exit
    | Lambda
    | Let
    | Display
    | Newline
    override this.ToString() =
        match this with
        | If -> "if"
        | Cond -> "cond"
        | Add -> "+"
        | Subtract -> "-"
        | Divide -> "/"
        | Multiply -> "*"
        | And -> "and"
        | Or -> "or"
        | Equal -> "="
        | GreaterThan -> ">"
        | LessThan -> "<"
        | GreaterThanOrEqual -> ">="
        | LessThanOrEqual -> "<="
        | Define -> "define"
        | Else -> "else"
        | Exit -> "exit"
        | Lambda -> "lambda"
        | Let -> "let"
        | Display -> "display"
        | Newline -> "newline"
    static member FromString = function
        | "if" -> If |> Some
        | "cond" -> Cond |> Some
        | "and" -> And |> Some
        | "or" -> Or |> Some
        | "define" -> Define |> Some
        | "else" -> Else |> Some
        | "exit" -> Exit |> Some
        | "lambda" -> Lambda |> Some
        | "let" -> Let |> Some
        | _ -> None