#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :gerbil/expander :utils/ports)
(set-current-ports-encoding-standard-unix!)
(current-directory (path-normalize (path-directory (this-source-file))))
(add-load-path (current-directory))
(import-module ':utils/t/unit-tests #t #t)
(def main (eval 'utils/t/unit-tests#main))
