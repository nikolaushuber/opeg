[@@@ocaml.warning "-37"]

module ParseResult : sig 
  type t = 
    | FullParse of string
    | PartialParse of string
    | NoParse 
end = struct 
  type t = 
    | FullParse of string
    | PartialParse of string
    | NoParse 
end 

module type Expr = sig 
  type t [@@deriving yojson]
  val eval : t -> ParseResult.t 
end

module rec ParseExpr : Expr = struct 
  type t = 
    | Match of MatchExpr.t 
    | Predicate of PredicateExpr.t 
    | Sequence of t * t 
    | Repetition of RepetitionExpr.t 

  let rec eval = function 
    | Match m -> MatchExpr.eval m 
    | Predicate p -> PredicateExpr.eval p 
    | Sequence (t1, t2) -> ignore (eval t1); eval t2 
    | Repetition r -> RepetitionExpr.eval r 
end

and RepetitionExpr : Expr = struct 
  type t = 
    | ZeroOrOne of ParseExpr.t  
    | ZeroOrMore of ParseExpr.t 
    | OneOrMore of ParseExpr.t 

  let eval = function 
    | ZeroOrOne p -> ParseExpr.eval p 
    | ZeroOrMore p -> ParseExpr.eval p 
    | OneOrMore p -> ParseExpr.eval p 
end 

and PredicateExpr : Expr = struct 
  type t = 
    | And of ParseExpr.t
    | Not of ParseExpr.t

  let eval = function 
    | And p -> ParseExpr.eval p 
    | Not p -> ParseExpr.eval p 
end

and MatchExpr : Expr = struct
 type t = 
    | Quote of string 
    | Regex of string 
  
  let eval : t -> ParseResult.t = function 
    | Quote _ -> NoParse 
    | Regex _ -> NoParse 
end

module Symbol = struct 
  type t = string 

  let to_string s = s 
  let of_string s = s 
  let compare s1 s2 = String.compare s1 s2 
end 

module SymbolSet = Set.Make (Symbol)

type named_expr = {
  name : Symbol.t; 
  expr : ParseExpr.t; 
}

type grammar = {
  start : Symbol.t; 
  symbols : SymbolSet.t; 
  rules : named_expr list; 
}

let (++) (g1 : grammar) (g2 : grammar) = {
  start = g1.start; 
  symbols = SymbolSet.union g1.symbols g2.symbols; 
  rules = []; 
}


