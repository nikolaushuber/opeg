# Opeg - _OCaml PEG parser generator_

`Opeg` is a parser generator for Parsing Expression Grammars (PEGs). It takes a description of a grammar in PEG style and turns it into OCaml code that can then be included into any OCaml application. The parsing interface is similar to [menhir]. 

## Tutorial 

Please have a look at the quickstart [tutorial](quickstart.md) to get an idea of how to use `Opeg`. 

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
 
