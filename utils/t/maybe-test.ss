(export maybe-test)

(import
  :gerbil/gambit/exceptions
  :std/test
  :clan/t/test-support
  :clan/utils/option :clan/utils/maybe)

(def maybe-test
  (test-suite "test suite for clan/utils/maybe"
    (test-case "test None"
      (check-eqv? None (void)))
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
      (check-equal? (with-catch error-exception-message (cut maybe-ref None)) "no value")
      (check-equal? (maybe-ref (some 5)) (some 5)))
    (test-case "test maybe-get"
      (check-equal? (maybe-get 1) 1)
      (check-equal? (maybe-get 1 42) 1)
      (check-equal? (maybe-get None) #f)
      (check-equal? (maybe-get None 42) 42)
      (check-equal? (maybe-get (some 5)) (some 5)))
    (test-case "test maybe-get/default"
      (check-equal? (maybe-get/default 1) 1)
      (check-equal? (maybe-get/default 1 true) 1)
      (check-equal? (maybe-get/default None) #f)
      (check-equal? (maybe-get/default None true) #t)
      (check-equal? (maybe-get/default (some 5)) (some 5)))
    (test-case "test map/maybe"
      (check-equal? (map/maybe list 1) [1])
      (check-equal? (map/maybe list None) None)
      (check-equal? (map/maybe list #f) [#f]))
    (test-case "test list<-maybe"
      (check-equal? (list<-maybe 1) [1])
      (check-equal? (list<-maybe None) []))
    (test-case "test list-map/maybe"
      (check-equal? (list-map/maybe (cut map/maybe 1+ <>) [2 5 11 23]) [3 6 12 24])
      (check-equal? (list-map/maybe (cut map/maybe 1+ <>) [2 5 None 11 23]) None))
    (test-case "test bind/maybe"
      (check-equal? (bind/maybe None 1+) None)
      (check-equal? (bind/maybe 3 1+) 4))
    (test-case "test iter/maybe"
      (check-equal? (with-output-to-string (cut iter/maybe display None)) "")
      (check-equal? (with-output-to-string (cut iter/maybe display (* 6 7))) "42"))
    ))
