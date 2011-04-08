module Refal.Refal

open System

open Refal.ParserCombinator
open Refal.RefalParser

let matchTerms terms =
    let termToParser term =
        match term with
        | SVar(i)       -> item >>= (fun c -> result [(term, [c])])
        | EVar(i)       -> many1 item >>= (fun cs -> result [(term, cs)])
        | Literal(s)    -> string s >>= (fun s -> result [(term, Seq.toList s)])
        | Funcall(_, _) -> failwith "Funcall in pattern? NO WAY!"

    let bad results =
        let doNotMatch a b = (fst a = fst b) && (snd a <> snd b)
        List.exists (fun a -> List.exists (doNotMatch a) results) results
    
    List.map termToParser terms |> List.reduce (++>)
    >> List.filter (snd >> List.isEmpty)
    >> List.map fst
    >> List.filter (bad >> not)
    >> List.map (Seq.distinct >> Seq.toList)


let rebuild lhs rhs =
    let rec getResult t =
        match t with
        | SVar(_) | EVar(_)     -> List.tryFind (fun p -> fst p = t) lhs
        | Literal(s)            -> Some(t, Seq.toList s)
        | Funcall(name, terms)  -> let args = List.map getResult terms
                                   if List.exists Option.isNone args
                                       then None
                                       else Some(Funcall(name, List.map Option.get args |> List.map fst), [])
    
    let results =  List.map getResult rhs
    results
//    if List.exists Option.isNone results
//        then None
//        else Some(List.map (Option.get >> snd) results |> List.concat)