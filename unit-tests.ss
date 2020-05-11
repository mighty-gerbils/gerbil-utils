#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :gerbil/expander :clan/utils/ports)
(set-current-ports-encoding-standard-unix!)
(current-directory (path-normalize (path-directory (this-source-file))))
(add-load-path (current-directory))
(import-module ':clan/t/unit-tests #t #t)
(def main (eval 'clan/t/unit-tests#main))
