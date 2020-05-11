(export option-test)

(import
  :gerbil/gambit/exceptions
  :std/test
  :clan/t/test-support
  :clan/utils/option)

(def option-test
  (test-suite "test suite for clan/utils/option"
    (test-case "test option-ref"
      (check-equal? (option-ref (some 1)) 1)
      (check-equal? (with-catch error-exception-message (cut option-ref #f)) "no value")
      (check-equal? (option-ref (some 5)) 5))
    (test-case "test option-get"
      (check-equal? (option-get (some 1)) 1)
      (check-equal? (option-get (some 1) 42) 1)
      (check-equal? (option-get #f) #f)
      (check-equal? (option-get #f 42) 42)
      (check-equal? (option-get 'also-none) #f)
      (check-equal? (option-get 'also-none 42) 42)
      (check-equal? (option-get (some 5)) 5))
    (test-case "test option-get/default"
      (check-equal? (option-get/default (some 1)) 1)
      (check-equal? (option-get/default (some 1) true) 1)
      (check-equal? (option-get/default #f) #f)
      (check-equal? (option-get/default #f true) #t)
      (check-equal? (option-get/default 'still-none) #f)
      (check-equal? (option-get/default 'still-none true) #t)
      (check-equal? (option-get/default (some 5)) 5))
    (test-case "test map/option"
      (check-equal? (map/option 1+ (some 1)) (some 2))
      (check-equal? (map/option 1+ #f) #f)
      (check-equal? (map/option list 'also-none) 'also-none)
      (check-equal? (map/option list (some #f)) (some [#f])))
    (test-case "test list<-option"
      (check-equal? (list<-option (some 1)) [1])
      (check-equal? (list<-option (some [1 2 3])) [[1 2 3]])
      (check-equal? (list<-option #f) [])
      (check-equal? (list<-option ['none 'none 'none]) []))
    (test-case "test list-map/option"
      (check-equal? (list-map/option (cut map/option 1+ <>) [(some 2) (some 5) (some 11)])
                    (some [3 6 12]))
      (check-equal? (list-map/option (cut map/option 1+ <>) [(some 2) (some 5) #f (some 11)]) #f))
    (test-case "test bind/option"
      (check-equal? (bind/option #f 1+) #f)
      (check-equal? (bind/option (some 3) 1+) 4))
    (test-case "test iter/option"
      (check-equal? (with-output-to-string (cut iter/option display #f)) "")
      (check-equal? (with-output-to-string (cut iter/option display (some (* 6 7)))) "42"))
    ))
