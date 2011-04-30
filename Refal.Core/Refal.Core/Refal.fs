module Refal.Refal

open System

open Refal.ParserCombinator
open Refal.RefalParser


let matchWithPattern pattern inp =
    /// Converts term to parser
    let termToParser term =
        match term with
        | SVar(i)       -> item >>= (fun c -> result [(term, [c])])
        | EVar(i)       -> many1 item >>= (fun cs -> result [(term, cs)])
        | Literal(s)    -> string s >>= (fun s -> result [(term, Seq.toList s)])
        | Funcall(_, _) -> failwith "Funcall in pattern? NO WAY!"

    let bad results =
        let doNotMatch a b = (fst a = fst b) && (snd a <> snd b)
        List.exists (fun a -> List.exists (fun b -> doNotMatch a b) results) results

    match pattern, inp with
    | [], []            -> [[]]     // magic
    | _,  [] | [], _    -> []       // more magic
    | otherwise ->
        inp
        |> (List.map termToParser pattern |> List.reduce (++>))
        |> List.filter (snd >> List.isEmpty)
        |> List.map fst
        |> List.filter (bad >> not)
        |> List.map (Seq.distinct >> Seq.toList)

let execProgram p programInput =
    
    let rec execFunc f args =
        
        let rebuild lhs rhs =
            let rec getResult t =
                match t with
                | SVar(_) | EVar(_)     -> match List.tryFind (fst >> (=) t) lhs with
                                           | None       -> None
                                           | Some(v)    -> Some(snd v)
                | Literal(s)            -> Some(Seq.toList s)
                | Funcall(name, terms)  -> let args = List.map getResult terms
                                           if List.exists Option.isNone args
                                               then None
                                               else let nextF = List.find (fst >> (=) name) p
                                                    let nextArgs = List.map Option.get args |> List.reduce (fun a b -> a @ [' '] @ b)
                                                    execFunc nextF nextArgs
            let nextRhs = List.map getResult rhs
            if List.exists Option.isNone nextRhs
                then None
                else Some(List.map Option.get nextRhs |> List.concat)


        let suitableSentence = snd f |> List.find (fun s -> not (List.isEmpty (matchWithPattern (fst s) args)))
        let lhs = matchWithPattern (fst suitableSentence) args
        rebuild (matchWithPattern (fst suitableSentence) args |> List.head) (snd suitableSentence)


    match List.tryFind (fst >> (=) "Go") p with
    | None      -> None
    | Some(f)   -> Some(execFunc f programInput)





let ExecuteProgram (p: String) (inp: String) =
    match execProgram (program (Seq.toList p) |> List.head |> fst) (Seq.toList inp) with
    | Some(Some(res))   -> String.ofChars res
    | otherwise         -> failwith "Some error occured. Maybe Refal code is wrong. Maybe you're just a loser."
