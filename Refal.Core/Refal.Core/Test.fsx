open System

#load "ParserCombinator.fs"
#load "RefalParser.fs"
#load "Refal.fs"

open Refal.ParserCombinator
open Refal.RefalParser
open Refal.Refal

let pat = (many term) (Seq.toList "'abcd' s.1 'f' s.1") |> List.head |> fst |> List.concat
let inp = Seq.toList "abcdefe"
let x = matchInp pat inp
