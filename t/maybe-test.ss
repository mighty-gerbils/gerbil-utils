(export maybe-test)

(import
  :gerbil/gambit/exceptions
  :std/test
  ./test-support
  ../option ../maybe)

(def maybe-test
  (test-suite "test suite for utils/maybe"
    (test-case "test null"
      (check-eqv? null (void)))
    (test-case "test Option<-Maybe, Maybe<-Option"
      (def t (match <> ([option maybe]
                        (check-equal? option (Option<-Maybe maybe))
                        (check-equal? maybe (Maybe<-Option option)))))
      (for-each t [[(some 1) 1]
                   [(some (some "foo")) (some "foo")]
                   [#f (void)]
                   [(some #f) #f]]))
    (test-case "test maybe-ref"
      (check-equal? (maybe-ref 1) 1)
      (check-equal? (with-catch error-exception-message (cut maybe-ref null)) "no value")
      (check-equal? (maybe-ref (some 5)) (some 5)))
    (test-case "test maybe-get"
      (check-equal? (maybe-get 1) 1)
      (check-equal? (maybe-get 1 42) 1)
      (check-equal? (maybe-get null) #f)
      (check-equal? (maybe-get null 42) 42)
      (check-equal? (maybe-get (some 5)) (some 5)))
    (test-case "test maybe-get/default"
      (check-equal? (maybe-get/default 1) 1)
      (check-equal? (maybe-get/default 1 true) 1)
      (check-equal? (maybe-get/default null) #f)
      (check-equal? (maybe-get/default null true) #t)
      (check-equal? (maybe-get/default (some 5)) (some 5)))
    (test-case "test map/maybe"
      (check-equal? (map/maybe list 1) [1])
      (check-equal? (map/maybe list null) null)
      (check-equal? (map/maybe list #f) [#f]))
    (test-case "test list<-maybe"
      (check-equal? (list<-maybe 1) [1])
      (check-equal? (list<-maybe null) []))
    (test-case "test list-map/maybe"
      (check-equal? (list-map/maybe (cut map/maybe 1+ <>) [2 5 11 23]) [3 6 12 24])
      (check-equal? (list-map/maybe (cut map/maybe 1+ <>) [2 5 null 11 23]) null))
    (test-case "test bind/maybe"
      (check-equal? (bind/maybe null 1+) null)
      (check-equal? (bind/maybe 3 1+) 4))
    (test-case "test for-each/maybe"
      (check-equal? (with-output-to-string (cut for-each/maybe display null)) "")
      (check-equal? (with-output-to-string (cut for-each/maybe display (* 6 7))) "42"))
    ))
