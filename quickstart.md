# Quickstart tutorial for Opeg

## Introduction 

`Opeg` is a tool that allows you to automatically generate parsers from a description of the grammar you want to parse. Therefore, you have to prepare your grammar in a specific way that `Opeg` understands. This tutorial will walk you through the creation of a simple example application that can read strings containing arithmetic expressions and subsequently evaluate them. 

## Prerequisites 

You need to have the following packages installed: 

- Opeg 
- OpegLib 

For information on how to install these please refer to [the README](README.md). 

In order to make sense of this tutorial it is assumed that you have a general understand of what parsing (in the context of programming languages) is. A certain familiarity with context free grammars is also necessary. If you have used [menhir](http://gallium.inria.fr/~fpottier/menhir/) before, that would be an asset. 

## Defining the grammar 

Roughly speaking, the grammar description file can be divided into three parts. The first one is an optional header, which can include arbitrary OCaml code. This code must be enclosed by matching `%{` and `%}` delimiters. Most often the header is used to include the module that holds the type definitions for the AST that the parser shall build up. In our very simple example we can define this type directly inside the header:

```ocaml 
%{
type ast = 
    | Add of ast * ast 
    | Sub of ast * ast 
    | Mul of ast * ast 
    | Div of ast * ast 
    | Num of int 
%}
```

We define the type `ast` which can be one of the primitive operations of arithmetics or a number. Each primitive operation has two subexpressions (which are themselves of type `ast`) which are the lefthandside and righthandsed subexpressions of that particular operation. 

The next part of the grammar file consists of the definition of the tokens of the language, and the name of the parser function: 

```ocaml 
token:
    | ADD "+" 
    | SUB "-" 
    | MUL "*" 
    | DIV "/" 
    | LPAREN "("  
    | RPAREN ")" 
    | NUMBER <int> 
    | EOF 

parser "parse" start 
```

As you can see, the tokens of our language are defined as a list. Each token definition has the form `| TOKEN_NAME <TOKEN_TYPE> "SHORTNAME"` where `<TOKEN_TYPE>` and `"SHORTNAME"` are optionl. The token type is the type of the optional lexeme a token can carry (here only the NUMBER token has a lexeme of type int, the actual number that was read). The shortname is an abbreviation that can be used when describing the derivation rules of the grammar, so whenever we expect an ADD token we can write "+" instead. This helps keeping the grammar rules concise and easy to read. 

The second part of the above code shows the definition of the generated parser function. It starts with the keyword `parser` followed by the name of that the toplevel parse function shall have (i.e. after compilation the generated parser will have a toplevel parse function `Parser.parse`). The last part of the parser definition is the name of the toplevel rule (i.e. the start symbol of our grammar). 

The third part of the grammar description file are the actual rules of the grammar: 

```ocaml 
%% 

start <ast>: 
    / e = expr EOF { e }

expr <ast>: 
    / t = term "+" e = expr { Add (t, e) }
    / t1 = term "-" t2 = term { Sub (t1, t2) }
    / t = term { t }

term <ast>:
    / a = atom "*" t = term { Mul (a, t) }
    / a1 = atom "/" a2 = atom { Div (a1, a2) }
    / a = atom { a } 

atom <ast>:
    / n = NUMBER { Num n }
    / "(" e = expr ")" { e }
```

As you can see, the rule part of the grammar description starts with the special symbol `%%` which serves as a divider from the token and parser interface description. Each rule follows the same pattern: 

```
rule_name <return type>:
    / alternative { action }
    / ... 
```

Each rule starts with the name of the rule, followed by the type of the value that this rule shall return upon a successful parse. In our grammar this is particularly easy, since all rules return a value of type `ast`. Each rule further has a list of alternatives, and each alternative is followed by a semantic action. Take for example the first alternative of the expr rule: 

```ocaml 
    / t = term "+" e = expr { Add (t, e) }
```

A rule always starts with the choice operator `/` followed by a list of both terminal and non-terminal symbols. If you want to use the return value (or lexeme) of a symbol you can assign it to a variable, as we have done above by recording the return value of the `term` and `expr` rules in variables `t` and `e` respectively. Afer the list of symbols follows the semantic action, which describes what to return when that specific alternative succeeds in parsing (a port of) the input. Here we are just creating an `Add` clause of the `ast` type we have defined in the header! You can also see how we can make use of the variables we have defined in the symbol list! 

If we put everything together, we end up with the following description of our grammar, which we save into a file `parser.peg`: 

```ocaml 
%{
type ast = 
    | Add of ast * ast 
    | Sub of ast * ast 
    | Mul of ast * ast 
    | Div of ast * ast 
    | Num of int 
%}

token:
    | ADD "+" 
    | SUB "-" 
    | MUL "*" 
    | DIV "/" 
    | LPAREN "("  
    | RPAREN ")" 
    | NUMBER <int> 
    | EOF 

parser "parse" toplevel 

%% 

toplevel <ast>: 
    / e = expr EOF { e }

expr <ast>: 
    / t = term "+" e = expr { Add (t, e) }
    / t1 = term "-" t2 = term { Sub (t1, t2) }
    / t = term { t }

term <ast>:
    / a = atom "*" t = term { Mul (a, t) }
    / a1 = atom "/" a2 = atom { Div (a1, a2) }
    / a = atom { a } 

atom <ast>:
    / n = NUMBER { Num n }
    / "(" e = expr ")" { e }
```

## The lexer 

The lexer code will go into `lexer.mll`: 

```ocaml 
{
  open Parser

  exception LexingError of string
}

let whitespace = [' ' '\t' '\r']+ 
let digit = ['0' - '9']
let integer = '0' | ['1'-'9'] (digit)* 

rule read_token = parse 
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "+" { ADD }
    | "-" { SUB }
    | "*" { MUL }
    | "/" { DIV }
    | whitespace { read_token lexbuf }
    | integer as i { NUMBER(int_of_string i) }
    | eof { EOF }
    | _ as c { raise (LexingError ("Lexer encountered unknown character: " ^ Char.escaped c)) }
``` 

There is nothing specific to Opeg here, please refer to the official documentation of [ocamllex](https://v2.ocaml.org/manual/lexyacc.html). 

## Putting it all together 

In order to play around with our language we can define some convenience functions in `arith.ml`: 

```ocaml 
let rec eval (expr : Parser.ast) : int = match expr with 
  | Num n -> n 
  | Add (e1, e2) -> Int.add (eval e1) (eval e2)
  | Sub (e1, e2) -> Int.sub (eval e1) (eval e2) 
  | Mul (e1, e2) -> Int.mul (eval e1) (eval e2) 
  | Div (e1, e2) -> Int.div (eval e1) (eval e2) 

let interpret (s : string) : int = 
  Lexing.from_string s 
  |> Parser.parse Lexer.read_token 
  |> eval 
``` 

As you can see, we have defined a recursive evaluator function for expressions of type `ast` which we defined above. We also included a function `interpret` which takes an arithmetic expression as a string and converts it to the evaluated integer. As you can see, the generated parser interface is the same as the one you get from [menhir](http://gallium.inria.fr/~fpottier/menhir/). 

## Using Dune 

In order to actually use the parser we have to first use `Opeg` to convert our grammar description into proper OCaml code. We could do this statically by now perfoming 

```sh 
opeg parser.peg -o parser.ml 
``` 

which would generate the code for us. However, most people will probably use the build system [Dune](https://dune.build) to create their applications. Therefore, we can get dune to automatically generate the parser code for us when building any artefact that makes use of the parser. For our little language the dune file should look like the following: 

```
(library
 (name arith)
 (libraries opegLib))

(rule 
 (target parser.ml)
 (deps parser.peg)
 (action (run opeg %{deps} -o %{target})))

(ocamllex lexer)
```

Please refer to [dune's documentation](https://dune.readthedocs.io) if you have troubles understanding the above code. 


