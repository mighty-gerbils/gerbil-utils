#!/usr/bin/env gxi

(import
  :std/test
  "all-tests")

(apply run-tests! unit-tests)
(test-report-summary!)

(case (test-result)
  ((OK) (exit 0))
  (else (exit 1)))
