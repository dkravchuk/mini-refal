module Refal.Refal

open System

open Refal.ParserCombinator
open Refal.RefalParser

let matchInp pattern input =
    let rec loop pat inp res =
        match List.isEmpty pat, List.isEmpty inp with
        | true, true                -> Some(res)
        | true, false | false, true -> None
        | false, false              ->
            let ph, pt = List.head pat, List.tail pat
            let ih, it = List.head inp, List.tail inp

            match ph with
            | Literal(c) -> if ih = c
                            then loop pt it res
                            else loop pat [] res

            | SVar(i)    -> match List.tryFind (fun x -> fst x = SVar(i)) res with
                            | None    -> loop pt it ((ph, [ih]) :: res)
                            | Some(c) -> if ih = List.head (snd c)
                                         then loop pt it res
                                         else loop pat [] res

            | otherwise -> loop pat [] res

    loop pattern input []