(export failure-test)

(import
  :std/test
  ../option
  ../failure)

(def failure-test
  (test-suite "test suite for clan/failure"
    (test-case "simple failure"
      (check (match (failure "foo") ((failure x) x)) => "foo")
      (check (match ((cut <> "foo") failure) ((failure) 2)) => 2)
      (check (failure? (failure "foo")) => #t)
      (check (failure? (some "foo")) => #f)
      (check (option? (failure "foo")) => #f) ;; failure is not an option
      (check (result? (some "foo")) => #t)
      (check (result? (failure "foo")) => #t)
      (check (result? "foo") => #f)
      (check (call/result (lambda () "foo")) => (some "foo"))
      (check (call/result (cut raise "foo")) => (failure "foo"))
      (check (with-result "foo") => (some "foo"))
      (check (with-result (raise "foo")) => (failure "foo"))
      (check (run-result (some "foo")) => "foo")
      (check-exception (run-result (failure "foo")) (cut equal? "foo" <>))
      (check-exception (run-result #f) failure?))))
