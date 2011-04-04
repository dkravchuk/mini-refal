open System

#load "ParserCombinator.fs"
#load "RefalParser.fs"
#load "Refal.fs"

open Refal.ParserCombinator
open Refal.RefalParser
open Refal.Refal

let sen = sentence (Seq.toList "s.1 e.1 s.1 = e.1;") |> List.head |> fst
let inp = Seq.toList "abcba"
let x = matchInp (fst sen) inp
let res = rebuild (Option.get x) (snd sen) |> Option.get |> List.map snd |> List.concat