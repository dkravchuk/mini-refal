open System

#load "ParserCombinator.fs"
#load "RefalParser.fs"
#load "Refal.fs"

open Refal.ParserCombinator
open Refal.RefalParser
open Refal.Refal

let sen = sentence (Seq.toList "s.1 e.1 s.1 = <Pal e.1 e.2>;") |> List.head |> fst
let inp = Seq.toList "abcba"
let pat = fst sen
let lhs = matchTerms pat inp |> List.head
let rhs = snd sen
let rebuilt = rebuild lhs rhs
