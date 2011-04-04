module Refal.Refal

open System

open Refal.ParserCombinator
open Refal.RefalParser

let matchInp pattern input =
    let rec loop pat inp res =
        match pat, inp with
        | [], []            -> Some(res)
        | [], _ | _, []     -> None
        | ph::pt, ih::it    ->
            match ph with

            | Literal(s) -> let inps = charsToString inp
                            if inps.StartsWith s
                                then loop pt (String.length s |> inps.Substring |> Seq.toList) res
                                else loop pat [] res

            | SVar(i)    -> match List.tryFind (fun x -> fst x = SVar(i)) res with
                            | None      -> loop pt it ((ph, [ih]) :: res)
                            | Some(c)   -> if ih = List.head (snd c)
                                               then loop pt it res
                                               else loop pat [] res

            | EVar(i)    -> let rec loop' inp' e =
                                match inp' with
                                | []        -> loop pat [] res
                                | ih'::it'  ->
                                    match loop pt inp' res with
                                    | None          -> loop' it' (ih' :: e)
                                    | Some(res'')   -> Some((ph, List.rev e) :: res'')
                            loop' (List.tail inp) [List.head inp]
                            
            | otherwise -> loop pat [] res

    loop pattern input []


let rebuild (lhs: (Term * Char list) list) (rhs: Term list) =
    let rec getResult t =
        match t with
        | Funcall(name, terms)  -> let args = List.map getResult terms
                                   if List.exists Option.isNone args
                                       then None
                                       else Some(Funcall(name, List.map Option.get args))
        | otherwise             -> List.tryFind (fun p -> fst p = t) lhs
    
    let results =  List.map getResult rhs
    if List.exists Option.isNone results
        then None
        else Some(List.map Option.get results)