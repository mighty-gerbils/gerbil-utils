(export symdict-test)

(import :std/test
        :std/error
        :clan/testing
        ../symdict
        ../assq)

(def symdict-test
  (test-suite "test suite for pure/dict/symdict"
    (check symdict=? (symdict) empty-symdict)
    (check-equal? (symdict->list empty-symdict) [])
    (check-equal? (symdict-empty? empty-symdict) #t)
    (check-equal? (symdict-empty? (symdict ('red 5))) #f)
    (check-equal? (symdict->list (symdict ('red  5))) '((red . 5)))

    (check-equal? (symdict-ref (symdict ('red 5) ('blue 4) ('black 1)) 'blue)
                  4)
    (check-equal? (symdict-ref (symdict ('red 5) ('blue 4) ('black 1)) 'red)
                  5)
    (check-equal? (symdict-ref (symdict ('red 5) ('blue 4) ('black 1)) 'black)
                  1)
    (check-exception (symdict-ref (symdict ('red 5) ('blue 4) ('black 1)) 'white)
                     (error-with-message? "symdict-ref: No value associated with key"))
    (check-equal? (symdict-get (symdict ('red 5) ('blue 4) ('black 1)) 'white)
                  #f)
    (check-equal? (symdict-get (symdict ('red 5) ('blue 4) ('black 1)) 'white 0)
                  0)

    (check symdict=?
           (symdict-put empty-symdict 'red [255 0 0])
           (list->symdict '((red . (255 0 0)))))
    (check symdict=?
           (symdict-put (symdict-put empty-symdict 'red [255 0 0]) 'blue [0 255 0])
           (list->symdict '((blue . (0 255 0)) (red . (255 0 0)))))
    (check symdict=?
           (symdict-put (symdict-put empty-symdict 'blue [0 255 0]) 'red [255 0 0])
           (list->symdict '((red . (255 0 0)) (blue . (0 255 0)))))

    (check symdict=?
           (symdict-update (symdict ('a 10) ('b 20) ('c 30)) 'b 1+ 0)
           (symdict ('b 21) ('a 10) ('c 30)))
    (check symdict=?
           (symdict-update (symdict ('a 10) ('b 20) ('c 30)) 'd 1+ 0)
           (symdict ('d 1) ('a 10) ('b 20) ('c 30)))

    (check symdict=?
           (symdict-remove (symdict ('a 10) ('b 20) ('c 30)) 'b)
           (symdict ('a 10) ('c 30)))
    (check symdict=?
           (symdict-remove (symdict ('a 10) ('b 20) ('c 30)) 'd)
           (symdict ('a 10) ('b 20) ('c 30)))

    (check-equal? (symdict-has-key? (symdict ('a 10) ('b 20) ('c 30)) 'a) #t)
    (check-equal? (symdict-has-key? (symdict ('a 10) ('b 20) ('c 30)) 'b) #t)
    (check-equal? (symdict-has-key? (symdict ('a 10) ('b 20) ('c 30)) 'c) #t)
    (check-equal? (symdict-has-key? (symdict ('a 10) ('b 20) ('c 30)) 'd) #f)

    (def (list-set=? as bs)
      (and (= (length as) (length bs))
           (andmap (lambda (a) (member a bs)) as)
           (andmap (lambda (b) (member b as)) bs)))

    (check list-set=?
           (symdict-keys (symdict ('red 5) ('blue 4) ('black 1)))
           ['red 'blue 'black])
    (check list-set=?
           (symdict-values (symdict ('red 5) ('blue 4) ('black 1)))
           [5 4 1])

    (check symdict=?
           (list->symdict '((blue . (0 255 0)) (red . (255 0 0))))
           (list->symdict '((red . (255 0 0)) (blue . (0 255 0)))))
    (check assq=?
           (symdict->list (list->symdict '((a . 1) (b . 2) (c . 3))))
           '((a . 1) (b . 2) (c . 3)))

    (check-equal? ((symdict->repr-sexpr values) (symdict ('a 10) ('b 20) ('c 30)))
                  '(symdict ('a 10) ('c 30) ('b 20)))
    (check symdict=?
           ((repr-sexpr->symdict values) '(symdict ('a 10) ('c 30) ('b 20)))
           (symdict ('a 10) ('b 20) ('c 30)))))
