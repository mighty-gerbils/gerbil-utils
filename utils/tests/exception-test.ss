(export exception-test)

(import :std/test
        :gerbil/gambit
        :clan/utils/exception)

(def (inside0 x) (cons 0 (inside1 x)))

(def (inside1 x) (cons 1 (inside2 x)))

(def (inside2 x) (cons 2 (inside3 x)))

(def (inside3 x) (cons 3 (error 'inside4 x)))

(def exception-test
  (test-suite "test suite for clan/utils/exception"

    (test-case "catch and invoke cont"
      (check-equal?
        (with-catch/cont
         (lambda (exn cont)
           (##continuation-return cont [4]))
         (lambda ()
           (inside0 "arg")))
        [0 1 2 3 4]))

    (test-case "catch and display in context"
      (def port (open-output-string))
      (check
        string-prefix?
        (string-append
         "*** ERROR IN clan/utils/tests/exception-test#inside1 -- inside4 \"arg\"\n"
         "0  clan/utils/tests/exception-test#inside1 \n"
         "1  exception-test__0#      \n")
        (with-catch/cont
         (lambda (exn cont)
           (display-exception-in-context exn cont port)
           (display-continuation-backtrace cont port)
           (get-output-string port))
         (lambda ()
           (inside0 "arg")))))))

