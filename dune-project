(lang dune 2.8)
(generate_opam_files true)

(using menhir 2.0)

(name pyast)
(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://github.com/thierry.martinez/pyast.git"))
(homepage "https://github.com/thierry.martinez/pyast")
(bug_reports "https://github.com/thierry.martinez/pyast")
(documentation "https://github.com/thierry.martinez/pyast")
(version "1.0.1")

(package
 (name pyast)
 (synopsis "Python AST")
 (description "\
Python AST
")
 (depends
  (ocaml (>= 4.03.0))
  (dune (>= 1.11.3))
  (pyml (>= 20210226))
  (refl (>= 0.4.0))
  (cmdliner (>= 1.0.4))
  (redirect (>= 0.1.1))
  (stdcompat (>= 10))
  (odoc (and :with-doc (>= 1.5.1)))))
