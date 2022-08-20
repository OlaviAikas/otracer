(lang dune 3.2)

(name otracer)

(generate_opam_files true)

(source
 (github OlaviAikas/otracer))

(authors "Olavi Äikäs")

(maintainers "Olavi Äikäs")

(license OPENDOMAIN)

(documentation https://url/to/documentation)

(package
 (name otracer)
 (synopsis "A basic raytracer (renderer)")
 (description "This is just a raytracer I'm writing in order
               to learn OCaml, and dune better.")
 (depends ocaml dune camlimages alcotest)
 (tags
  (graphics rendering raytracing)
 ))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
