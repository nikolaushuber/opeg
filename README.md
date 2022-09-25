# Opeg - _OCaml PEG parser generator_

`Opeg` is a parser generator for Parsing Expression Grammars (PEGs). It takes a description of a grammar in PEG style and turns it into OCaml code that can then be included into any OCaml application. The parsing interface is similar to [menhir]. 

## Tutorial 

Please have a look at the quickstart [tutorial](quickstart.md) to get an idea of how to use `Opeg`. Once you have understood it you can have a look at the grammar file describing the meta-grammar (i.e. the grammar of grammar files) in [parser/parser.peg](parser/parser.peg)

## Installation

There are two components to parser generation with `Opeg`. The tool `opeg` is the actual parser geneator. The generated code will make calls to functions from `opegLib`. Both can be installed through [opam](https://opam.ocaml.org): 

```sh
opam pin add opegLib https://github.com/nikolaushuber/opeg.git -y
opam pin add opeg https://github.com/nikolaushuber/opeg.git -y
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

If no output is defined, no code will be generated. 

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

## Todo 

- ~~Bootstrap the grammar parser~~
- ~~Include convenience operators like ?, +, *~~
- ~~Add support for left recursion~~ (Direct left recursion supported)
- Add detection and warning for other types of left recursion (or eventually support other types)
- Evaluate performance against [menhir] 
- Try and see if we can use OCaml's Lazy module instead of hashtables 
- Tune initial hashtable size   
- Add an option to statically copy the opegLib code into the generated parser

## License

MIT

[//]: # (These are reference links used in the body of this note and get stripped out when the markdown processor does its job. There is no need to format nicely because it shouldn't be seen. Thanks SO - http://stackoverflow.com/questions/4823468/store-comments-in-markdown-syntax)

   [menhir]: <http://gallium.inria.fr/~fpottier/menhir/>
 
