(export exception-test)

(import
  :gerbil/gambit
  :std/format :std/test :std/srfi/13
  ../exception)

(def (inside0 x) (cons 0 (inside1 x)))

(def (inside1 x) (cons 1 (inside2 x)))

(def (inside2 x) (cons 2 (inside3 x)))

(def (inside3 x) (cons 3 (error 'inside4 x)))

(def exception-test
  (test-suite "test suite for clan/exception"

    (test-case "catch and invoke cont"
      (check-equal?
        (with-catch/cont
         (lambda (exn cont)
           (##continuation-return cont [4]))
         (lambda ()
           (inside0 "arg")))
        [0 1 2 3 4]))

    (test-case "catch and display in context"
      (defvalues (exn cont)
        (with-catch/cont values (lambda () (inside0 "arg"))))
      (def exn-in-ctx (call-with-output-string (lambda (port) (display-exception-in-context exn cont port))))
      (def backtrace (call-with-output-string (lambda (port) (display-continuation-backtrace cont port))))

      (eprintf "~%~%FOO FOO FOO FOO~%~s~%BAR BAR BAR BAR~%~s~%BAZ BAZ BAZ BAZ~%"
               exn-in-ctx backtrace)
      (check
       exn-in-ctx ?
       (lambda (x) (string-prefix? "*** ERROR IN clan/t/exception-test#inside3, \"t/exception-test.ss\"@14.26 -- \n*** ERROR IN ? [Error]: inside4\n--- irritants: arg \n--- continuation backtrace:\n" x)))
      (check
       backtrace ?
       (lambda (x) (string-prefix? "0  clan/t/exception-test#" x))))
    ))
