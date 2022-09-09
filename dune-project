(lang dune 3.4)
(using menhir 2.0)

(name opeg)

(generate_opam_files true)

(source
 (github nikolaushuber/opeg))

(authors "Nikolaus Huber")

(maintainers "Nikolaus Huber")

(license MIT)

(documentation https://github.com/nikolaushuber/opeg)

(package
 (name opeg)
 (synopsis "PEG parser generator")
 (description "PEG parser generator for OCaml")
 (depends ocaml dune)
 (tags
  (compiler parsing)))

(package 
  (name opeg_boot)
  (synopsis "Bootstrapping PEG parser generator")
  (description "This PEG parser generator is used to boostrap Opeg")
  (depends ocaml dune menhir)
  (tags
    (compiler parsing)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
