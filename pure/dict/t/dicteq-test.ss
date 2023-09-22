(export dicteq-test)

(import :std/test
        :std/error
        :clan/testing
        ../assq
        ../dicteq)

(def (error-with-message? message)
  (lambda (e)
    (and (Error? e) (equal? (Error-message e) message))))

(def dicteq-test
  (test-suite "test suite for pure/dict/dicteq"
    (check dicteq=? (dicteq) empty-dicteq)
    (check-equal? (dicteq->list empty-dicteq) [])
    (check-equal? (dicteq-empty? empty-dicteq) #t)
    (check-equal? (dicteq-empty? (dicteq ('red 5))) #f)
    (check-equal? (dicteq->list (dicteq ('red 5))) '((red . 5)))

    (check-equal? (dicteq-ref (dicteq ('red 5) ('blue 4) ('black 1)) 'blue)
                  4)
    (check-equal? (dicteq-ref (dicteq ('red 5) ('blue 4) ('black 1)) 'red)
                  5)
    (check-equal? (dicteq-ref (dicteq ('red 5) ('blue 4) ('black 1)) 'black)
                  1)
    (check-equal? (dicteq-ref (dicteq ('red 5) ('blue 4) ('black 1)) 'green (lambda () 'go-fish))
                  'go-fish)
    (check-equal? (dicteq-get (dicteq ('red 5) ('blue 4) ('black 1)) 'green 'go-fish)
                  'go-fish)
    (check-equal? (dicteq-get (dicteq ('red 5) ('blue 4) ('black 1)) 'green)
                  #f)
    (check-exception (dicteq-ref (dicteq ('red 5) ('blue 4) ('black 1)) 'green)
                     (error-with-message? "dicteq-ref: No value associated with key"))

    (check dicteq=?
           (dicteq-put empty-dicteq 'red [255 0 0])
           (list->dicteq '((red . (255 0 0)))))
    (check dicteq=?
           (dicteq-put (dicteq-put empty-dicteq 'red [255 0 0]) 'blue [0 255 0])
           (list->dicteq '((red . (255 0 0)) (blue . (0 255 0)))))
    (check dicteq=?
           (dicteq-put (dicteq-put empty-dicteq 'blue [0 255 0]) 'red [255 0 0])
           (list->dicteq '((red . (255 0 0)) (blue . (0 255 0)))))

    (check dicteq=?
           (dicteq-update (dicteq ('a 10) ('b 20) ('c 30)) 'b 1+ 0)
           (dicteq ('b 21) ('a 10) ('c 30)))
    (check dicteq=?
           (dicteq-update (dicteq ('a 10) ('b 20) ('c 30)) 'd 1+ 0)
           (dicteq ('d 1) ('a 10) ('b 20) ('c 30)))

    (check dicteq=?
           (dicteq-remove (dicteq ('a 10) ('b 20) ('c 30)) 'b)
           (dicteq ('a 10) ('c 30)))
    (check dicteq=?
           (dicteq-remove (dicteq ('a 10) ('b 20) ('c 30)) 'd)
           (dicteq ('a 10) ('b 20) ('c 30)))

    (check-equal? (dicteq-has-key? (dicteq ('a 10) ('b 20) ('c 30)) 'a) #t)
    (check-equal? (dicteq-has-key? (dicteq ('a 10) ('b 20) ('c 30)) 'b) #t)
    (check-equal? (dicteq-has-key? (dicteq ('a 10) ('b 20) ('c 30)) 'c) #t)
    (check-equal? (dicteq-has-key? (dicteq ('a 10) ('b 20) ('c 30)) 'd) #f)

    (def (list-set=? as bs)
      (and (andmap (lambda (a) (member a bs)) as)
           (andmap (lambda (b) (member b as)) bs)))

    (check list-set=?
           (dicteq-keys (dicteq ('red 5) ('blue 4) ('black 1)))
           ['red 'blue 'black])
    (check list-set=?
           (dicteq-values (dicteq ('red 5) ('blue 4) ('black 1)))
           [5 4 1])

    (check dicteq=?
           (list->dicteq '((blue . (0 255 0)) (red . (255 0 0))))
           (list->dicteq '((red . (255 0 0)) (blue . (0 255 0)))))
    (check assq=?
           (dicteq->list (list->dicteq '((a . 1) (b . 2) (c . 3))))
           '((a . 1) (b . 2) (c . 3)))

    (check-equal? ((dicteq->repr-sexpr values values) (dicteq ('a 10) ('b 20) ('c 30)))
                  '(dicteq (b 20) (c 30) (a 10)))
    (check dicteq=?
           ((repr-sexpr->dicteq values values) '(dicteq (b 20) (c 30) (a 10)))
           (dicteq ('a 10) ('b 20) ('c 30)))))
