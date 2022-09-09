# Opeg - _OCaml PEG parser generator_

Opeg is a parser generator for Parsing Expression Grammars (PEGs). It takes a description of a grammar in PEG style and turns it into OCaml code that can then be included into any OCaml application. The parsing interface is similar to [menhir]. 

## Example

```txt 
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

parser "parse" start 

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
    / NUMBER<n> { Num n }
    / "(" e = expr ")" { e }
```

The overall syntax is similar to [menhir], however, the semantic of the grammar is following the PEG paradigm. For each rule that has multiple alternatives, each of them is tried in turn, and the first that matches will succeed. This makes PEGs unambigouse by design.  

## Installation

You can install Opeg through [opam](https://opam.ocaml.org): 
```sh
opam pin add opeg https://github.com/nikolaushuber/opeg.git
```

Once Opeg has reached a certain stability, it will also be available through the regular opam package repository. 

## Usage 

```sh
opeg [file] -o [output]
```

If your grammar is in grammar.peg, then the following will produce the file parser.ml in the same directory:

```sh
opeg grammar.peg -o parser
```

If no output is defined, the generated file will have the same name as the input file. 

## Development 

This repository defines two different tools, *opeg* and *opeg_boot*. *Opeg_boot* is used for bootstrapping 
the parser generator, it uses [menhir] to parse the grammar specification. 

In order to start developing, you can do the following: 

```sh
git clone https://github.com/nikolaushuber/opeg.git  
cd opeg 
opam switch create . ocaml-base-compiler.4.14.0 -y 
opam pin . -y 
```

This will install all dependencies and create both *opeg* and *opeg_boot*. 

## License

MIT

[//]: # (These are reference links used in the body of this note and get stripped out when the markdown processor does its job. There is no need to format nicely because it shouldn't be seen. Thanks SO - http://stackoverflow.com/questions/4823468/store-comments-in-markdown-syntax)

   [menhir]: <http://gallium.inria.fr/~fpottier/menhir/>
 
