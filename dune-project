(lang dune 2.4)
(name ur)

(generate_opam_files)

(source (github Firobe/Ur))
(authors "Virgile Robles")
(maintainers "Virgile Robles")

(package
  (name ur)
  (synopsis "A graphical implementation of the royal game of Ur")
  (description "A graphical implementation of the royal game of Ur")
  (depends
    base-threads
    tsdl
    tgls
  )
)
