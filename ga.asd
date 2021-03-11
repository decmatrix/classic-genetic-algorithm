(asdf:defsystem :ga
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :description "Classic genetic algorithm in Common Lisp"
    :version "0.1"
    :author "Sokolovskyi Bohdan"
    :depends-on ("ga/ga-impl"))
