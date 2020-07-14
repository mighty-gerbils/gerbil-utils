(export number-test)

(import
  :gerbil/gambit/exceptions
  :std/test
  ../base ../number)

(def (fit-to-string n width)
  (call-with-output-string []
    (λ (out) (display-integer/fit n width out))))

(def number-test
  (test-suite "test suite for utils/number"
    (test-case "display-integer/fit positive integer with extra width"
      (check-equal? (fit-to-string 5 5) "00005"))
    (test-case "display-integer/fit 0 with extra width"
      (check-equal? (fit-to-string 0 5) "00000"))
    (test-case "display-integer/fit negative integer"
      (check-exception (fit-to-string -5 5) error-exception?))
    (test-case "display-integer/fit zero width"
      (check-exception (fit-to-string 7 0) error-exception?))
    (test-case "display-integer/fit negative width"
      (check-exception (fit-to-string 7 -1) error-exception?))
    (test-case "display-integer/fit positive integer perfect fit"
      (check-equal? (fit-to-string 12345 5) "12345"))
    (test-case "display-integer/fit positive integer with insufficient width"
      (check-exception (fit-to-string 123456 5) error-exception?))))
