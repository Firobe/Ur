(lang dune 2.4)
(name ur)

(source (github Firobe/Ur))
(authors "Virgile Robles")
(maintainers "Virgile Robles")

(package
  (name ur)
  (synopsis "A graphical implementation of the royal game of Ur")
  (description "A graphical implementation of the royal game of Ur")
  (depends
    core
    base-threads
    tsdl
    tgls
  )
)
