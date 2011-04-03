module Refal.ParserCombinator

open System

/// Joins the specified Char list into a single String
let charsToString (chars: Char list) =
    String.Join ("", chars)


//--- Basic parsers ----------------------------------------

/// Basic always-succeeding parser
let result v inp = [(v, inp)]

/// Basic always-failing parser
let zero inp = []


//--- Combinators ------------------------------------------

/// Bind combinator
let (>>=) p f =
    p >> List.map (fun (v, rest) -> f v rest) >> List.concat

/// Sequence combinator
let (>>>) p q =
    p >>= fun _ ->
    q >>= fun v ->
    result v

/// Choice combinator
let (+++) p q inp =
//    p inp @ q inp
    match p inp with
    | []    -> q inp
    | res   -> res

/// Repetition combinators
let rec many1 p =
    p >>= fun h ->
    (many1 p +++ result []) >>= fun t ->
    result (h::t)
let many p = many1 p +++ result []

///// Separation combinators
//let rec sepby1 sep p =
//    p >>= fun h ->
//    many (sep >>> p >>= result) >>= fun t ->
//    result (h::t)
//let sepby sep p = sepby1 sep p +++ result []

/// Magic
let repeatUntil p q =
    let rec loop acc inp =
        match q inp with
        | [(v, rest)]   -> result (acc @ [v]) rest
        | []            -> match p inp with
                           | [(v, rest)]    -> loop (acc @ [v]) rest
                           | []             -> []
    
    p >>= fun h ->
    loop [] >>= fun t ->
    result (h::t)


//--- Simple parsers ---------------------------------------

/// Single item parser
let item inp =
    match inp with
    | []    -> []
    | h::t  -> result h t

/// Specified item parser
let sat p =
    item >>= fun x ->
    if p x then result x else zero

let char c = sat ((=) c)
let notchar c = sat ((<>) c)
let digit = sat Char.IsDigit
let letter = sat Char.IsLetter
let alphanum = sat Char.IsLetterOrDigit

/// Natural number parser
let nat =
    let eval =
        List.map (fun c -> int c - int '0') >>
        List.reduce (fun m n -> 10*m + n)
    many1 digit >>= fun digits ->
    result (eval digits)

/// Specified string parser
let string s =
    let rec loop chars =
        match chars with
        | []    -> result ""
        | h::t  -> char h >>>
                   loop t >>>
                   result (h::t |> charsToString)
    loop (Seq.toList s)