(lang dune 3.16)

(name fp_lab_1)

(generate_opam_files true)

(license BSD-3-Clause)

(package
 (name fp_lab_1)
 (depends
  (alcotest :with-test)
  (ocamlformat
   (and
    (= 0.26.2)
    :with-dev-setup))
  benchmark
  zanuda
))
