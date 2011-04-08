module Refal.RefalParser

#nowarn "40"

open System
open Refal.ParserCombinator

let spaces = char ' ' +++ char '\t' +++ char '\n'
let unjunk p =
    p >>= fun v ->
    many spaces >>>
    result v

/// Identifier parser
let identifier =
    letter >>= fun h ->
    many alphanum >>= fun t ->
    result (h::t |> String.ofChars)
    |> unjunk

/// Refal term
type Term =
    | Literal of String
    | SVar of int
    | EVar of int
    | Funcall of String * Term list

/// Refal literal string parser
let literal =
    char '\'' >>>
    many (notchar '\'') >>= fun items ->
    char '\'' >>>
    result (Literal(String.ofChars items))
    |> unjunk

/// S/E-type variable parser
let var =       
    (char 's' >>> result SVar) +++ (char 'e' >>> result EVar) >>= fun tp ->
    char '.' >>>
    nat >>= fun id ->
    result (tp(id))
    |> unjunk

/// Function call parser
let rec funcall =
            char '<' >>>
            identifier >>= fun name ->
            many term >>= fun args ->
            char '>' >>>
            result (Funcall(name, args))
            |> unjunk

/// Single Refal term parser
    and term = literal +++ var +++ funcall

/// Refal sentence parser
let sentence =
    many term >>= fun lhs ->
    unjunk (char '=') >>>
    many term >>= fun rhs ->
    char ';' >>>
    result (lhs, rhs)
    |> unjunk

/// Refal function parser
let func =
    identifier >>= fun name ->
    unjunk (char '{') >>>
    many sentence >>= fun body ->
    unjunk (char '}') >>>
    result (name, body)

/// Refal program
let program = many spaces >>> many func