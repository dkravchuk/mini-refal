open System

#load "ParserCombinator.fs"
#load "RefalParser.fs"
#load "Refal.fs"

open Refal.ParserCombinator
open Refal.RefalParser
open Refal.Refal

let progtext = @"
Go {
    s.1 s.1 = 'True';
    s.1 e.1 s.1 = <Go e.1;
    s.1 = 'True';
    = 'True';
    e.1 = 'False';
}"

let res = ExecuteProgram progtext "abcba"
