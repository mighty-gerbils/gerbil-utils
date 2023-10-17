;; At a test's runtime, locate test files from a package's separately-installed source code,
;; wherein the test files are not part of the binary installation of the package being tested.
;;
;; TODO: replace this with something that just uses Gerbil's load-path mechanism
;; (as controllable by GERBIL_LOADPATH and GERBIL_PATH),
;; possibly as a variant of gx-gambc0's find-library-module?
;;
;; Given a String for the name of a package in the gxpkg namespace, and
;; a String for a subpath relative to the package source,
;; return a path for that file, located either
;; (a) under the application source-path (see path-config) so if you're currently
;; developing and testing or debugging that package it will find it
;; *or an override from whatever package you're testing or debugging instead*, or
;; (b) under the package source code as configured in the GERBIL_PATH (which defaults to
;; ~/.gerbil/pkg), so if you're developing and testing another application,
;; it will find where you installed or linked gerbil-ethereum with gxpkg.
;; If not found in either place, issue an error.
;;
;; Note that binary packages typically do not include test files, and you're indeed supposed
;; to install and configure a package's source code to run some integration tests.
;;
;; Also note that this "override" mechanism assumes that there will be no name conflict
;; between relative paths referenced at runtime, between the toplevel package
;; from which the tests are run, and the package containing the test being run.
;; By running tests across package boundaries, we assume that the packages are part of a common
;; collection or hierarchy of packages, wherein the developers will address any namespace conflict.
;;
(import
  (only-in :std/srfi/141 floor/)
  (only-in :std/misc/path subpath)
  (only-in ./base ignore-errors)
  (only-in ./path-config source-path))

(export #t)

;; : String <- String String
(def (find-source-file package test-path)
  (def (try p) (ignore-errors (and (file-exists? p) p)))
  (or (try (source-path test-path))
      (try (subpath (or (getenv "GERBIL_PATH" #f) (path-expand "~/.gerbil"))
                    "pkg" package test-path))
      (error 'find-source-file package test-path)))

;; Explain position returned from Gambit location
;; <- Integer
(def (explain-position n)
  (if (positive? n)
    (let-values (((q r) (floor/ n 65536)))
      [line: (1+ r) column: q])
    [file-position: (- n)]))

;; Explain Gambit location
(def (explain-location l)
  (match l
    ((vector input position) [(explain-position position) ... in: input])
    (else l)))

;; TODO: experiment with:
;; (parameterize ((current-expander-phi (1+ (current-expander-phi))) (eval-syntax ...))
