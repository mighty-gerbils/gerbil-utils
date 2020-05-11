(export peekable-iterator-test)

(import
  :std/test
  :std/iter
  :clan/utils/peekable-iterator)

(def (iter-abcde)
  (:peekable-iter '(a b c d e)))

(def peekable-iterator-test
  (test-suite "test suite for clan/utils/peekable-iterator"
    (test-case "test extremum<-list"
      (def it (iter-abcde))
      (check-equal? (peek it) 'a)
      (check-equal? (end? it) #f)
      (check-equal? (next! it) 'a)
      (check-equal? (peek it) 'b)
      (check-equal? (end? it) #f)
      (check-equal? (next! it) 'b)
      (check-equal? (peek it) 'c)
      (check-equal? (end? it) #f)
      (check-equal? (next! it) 'c)
      (check-equal? (peek it) 'd)
      (check-equal? (end? it) #f)
      (check-equal? (next! it) 'd)
      (check-equal? (peek it) 'e)
      (check-equal? (end? it) #f)
      (check-equal? (next! it) 'e)
      (check-equal? (peek it) iter-end)
      (check-equal? (end? it) #t)
      (check-equal? (next! it) iter-end)
      (check-equal? (peek it) iter-end)
      (check-equal? (end? it) #t)
      (check-equal? (next! it) iter-end)
      (check-equal? (peek it) iter-end)
      (check-equal? (end? it) #t)
      (check-equal? (next! it) iter-end)
      (check-equal? (peek it) iter-end)
      (check-equal? (end? it) #t)
      (check-equal? (next! it) iter-end))))
