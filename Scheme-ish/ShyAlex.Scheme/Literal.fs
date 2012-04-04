module Literal

open System

type literal =
    | Nil
    | Number of float
    | Boolean of bool
    | Chr of char
    | Str of string
    override this.ToString() =
        match this with
        | Nil -> "#nil"
        | Number(n) -> n.ToString()
        | Boolean(b) -> if b then "#t" else "#f"
        | Chr(c) -> sprintf @"\#%O" c
        | Str(s) -> s.Replace("\r", @"\r").Replace("\n", @"\n")
    static member FromString = function
        | "#t" -> Boolean(true) |> Some
        | "#f" -> Boolean(false) |> Some
        | s -> let d = ref 0.0
               match Double.TryParse(s, d) with
               | true -> Some(Number(!d))
               | false -> match s.StartsWith(@"\#") && s.Length > 2 with
                          | true -> Some(Chr(s.[2]))
                          | false -> match s.StartsWith("\"") && s.EndsWith("\"") with
                                     | true -> Some(Str(s))
                                     | false -> None