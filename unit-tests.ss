#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :clan/t/test-support)
(def a 1)
(begin (init-test-environment! a))
(import :clan/version)
