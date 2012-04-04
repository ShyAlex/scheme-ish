module Env

open System.Collections.Generic

type 'a env() =
    let exprs = Dictionary<string, 'a list * 'a list>()
    member private this.Exprs = exprs
    member this.GetVar name = 
        let func = ref ([], [])
        match exprs.TryGetValue(name, func) with
        | true -> Some(!func)
        | false -> None
    member this.Copy() =
        let copy = env()
        for kvp in exprs do copy.Exprs.[kvp.Key] <- kvp.Value
        copy
    member this.SetVar name func = 
        let copy = this.Copy()
        copy.Exprs.[name] <- func
        copy
    member this.Combine (other:'a env) =
        let newEnv = this.Copy()
        for kvp in other.Exprs do newEnv.Exprs.[kvp.Key] <- kvp.Value
        newEnv
    member this.VarNames = exprs.Keys |> Seq.toList
    override this.ToString() =
        if exprs.Count < 1 then "[empty environment]\r\n" else
        exprs |> Seq.fold (fun s kvp -> s + (sprintf "%O | %O\r\n" kvp.Key kvp.Value)) ""