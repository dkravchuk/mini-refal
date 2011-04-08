module Refal.ParserCombinator

open System

module String =
    /// Joins the specified Char list into a single String
    let ofChars (chars: seq<Char>) = String.Join("", chars)

//--- Basic parsers ----------------------------------------

/// Basic always-succeeding parser
let result v inp = [(v, inp)]

/// Basic always-failing parser
let zero inp = []


//--- Combinators ------------------------------------------

/// Bind combinator
let (>>=) p f =
    p >> List.map (fun (v, rest) -> f v rest) >> List.concat

/// Sequence combinators
let (>>>) p q =
    p >>= fun _ ->
    q >>= fun v ->
    result v

let (++>) p q =
    p >>= fun vp ->
    q >>= fun vq ->
    result (vp @ vq)

/// Choice combinator
let (+++) p q inp =
    p inp @ q inp

/// Repetition combinators
let rec many1 p =
    p >>= fun h ->
    (many1 p +++ result []) >>= fun t ->
    result (h::t)

let many p = many1 p +++ result []


//--- Simple parsers ---------------------------------------

/// Single item parser
let item inp =
    match inp with
    | []    -> []
    | h::t  -> result h t

/// Empty list parser
let empty inp =
    match inp with
    | []        -> result [] []
    | otherwise -> []

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
                   result (h::t |> String.ofChars)
    loop (Seq.toList s)