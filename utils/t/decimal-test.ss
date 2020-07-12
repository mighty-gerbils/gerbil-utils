(export decimal-test)

(import
  :gerbil/gambit/exceptions
  :std/misc/string :std/srfi/13
  :std/test
  ../base ../basic-parsers ../decimal)

(def decimal-test
  (test-suite "test suite for clan/utils/decimal"
    (test-case "parse decimal"
      (for-each
        (match <> ([r . a] (check-equal? (apply decimal<-string a) r)))
        '((0 "0")
          (0 "0.0")
          (0 "0.")
          (0 ".0")
          (157/50 "3.14")
          (317000 "317000")
          (3170000 "3.17e6" exponent-allowed: #t)
          (317/100000000 "3.17e-6" exponent-allowed: #t)
          (-317/100000000 "-3.17e-6" exponent-allowed: #t))))
    (test-case "parse decimal error"
      (for-each
        (lambda (a) (check-exception (apply decimal<-string a) parse-error?))
        '((".")
          ("3.17e6")
          ("3.17e" exponent-allowed: #t)
          ("-3.17e-6" exponent-allowed: #t sign-allowed?: #f)
          ("-"))))
    (test-case "print decimal"
      (for-each
        (match <> ([r n . a]
                   (def x (apply string<-decimal n a))
                   (check-equal? x r)))
        '(("0" 0)
          ("3.14" 157/50)
          ("42." 42 always-decimal?: #t)
          ("1.0" 1 always-decimal?: ".0")
          ("+317000" 317000 always-sign?: #t)
          ("-0.00000317" -317/100000000)
          ("3.14" 314 scale: -2)
          ("               3.140" 314 width: 20 scale: -2 fractional-digits: 3)
          ("  3.14" 314/1000 width: 6 scale: 1 fractional-digits: 2)
          (".314" 314/1000 leading-decimal-mark-allowed?: #t))))
    (test-case "print decimal error"
      (for-each
        (lambda (a) (check-exception (apply string<-decimal a) disallowed-loss-of-precision?))
        '((317/100 width: 2))))
    ))
