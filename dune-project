(lang dune 3.16)

(name fp_lab_1)

(generate_opam_files true)

(license BSD-3-Clause)

(authors
 "Mark Efremov <jefremovmark@gmail.com>")

(maintainers
 "Mark Efremov <jefremovmark@gmail.com>")

(source
 (github jefremof/FP-lab-1))

(bug_reports "https://github.com/jefremof/FP-lab-1/issues")

(homepage "https://github.com/jefremof/FP-lab-1")

(package
 (name fp_lab_1)

 (synopsis "Laboratory focused on OCaml basics and principles of functional programming.")

 (allow_empty)
 (depends
  alcotest
  (ocamlformat
   (and
    (= 0.26.2)
    :with-dev-setup))
  (iter (= 1.6))
  benchmark
  zanuda
))
