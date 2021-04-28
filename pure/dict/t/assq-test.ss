(export assq-test)

(import :std/test
        :gerbil/gambit/exceptions
        ../assq)

(def (error-with-message? message)
  (lambda (e)
    (and (error-exception? e) (equal? (error-exception-message e) message))))

(def assq-test
  (test-suite "test suite for pure/dict/assq"
    (check-equal? (list->assq []) [])
    (check-equal? (list->assq '((red . 5))) '((red . 5)))

    (check-equal? (assq-ref '((red . 5) (blue . 4) (black . 1)) 'blue)
                4)
    (check-equal? (assq-ref '((red . 5) (blue . 4) (black . 1)) 'red)
                5)
    (check-equal? (assq-ref '((red . 5) (blue . 4) (black . 1)) 'black)
                1)
    (check-exception (assq-ref '((red . 5) (blue . 4) (black . 1)) 'white)
                     (error-with-message? "assq-ref: No value associated with key"))
    (check-equal? (assq-get '((red . 5) (blue . 4) (black . 1)) 'white)
                  #f)
    (check-equal? (assq-get '((red . 5) (blue . 4) (black . 1)) 'white 0)
                  0)

    (check-equal? (assq-put [] 'red [255 0 0])
                '((red . (255 0 0))))
    (check-equal? (assq-put (assq-put [] 'red [255 0 0]) 'blue [0 255 0])
                '((blue . (0 255 0)) (red . (255 0 0))))
    (check-equal? (assq-put (assq-put [] 'blue [0 255 0]) 'red [255 0 0])
                '((red . (255 0 0)) (blue . (0 255 0))))

    (check-equal? (assq-update '((a . 10) (b . 20) (c . 30)) 'b 1+ 0)
                '((b . 21) (a . 10) (c . 30)))
    (check-equal? (assq-update '((a . 10) (b . 20) (c . 30)) 'd 1+ 0)
                '((d . 1) (a . 10) (b . 20) (c . 30)))

    (check-equal? (assq-remove '((a . 10) (b . 20) (c . 30)) 'b)
                '((a . 10) (c . 30)))
    (check-equal? (assq-remove '((a . 10) (b . 20) (c . 30)) 'd)
                '((a . 10) (b . 20) (c . 30)))

    (check-equal? (assq-has-key? '((a . 10) (b . 20) (c . 30)) 'a) #t)
    (check-equal? (assq-has-key? '((a . 10) (b . 20) (c . 30)) 'b) #t)
    (check-equal? (assq-has-key? '((a . 10) (b . 20) (c . 30)) 'c) #t)
    (check-equal? (assq-has-key? '((a . 10) (b . 20) (c . 30)) 'd) #f)

    (check-equal? (assq-keys '((red . 5) (blue . 4) (black . 1)))
                ['red 'blue 'black])
    (check-equal? (assq-values '((red . 5) (blue . 4) (black . 1)))
                [5 4 1])

    (check assq=?
        '((blue . (0 255 0)) (red . (255 0 0)))
        '((red . (255 0 0)) (blue . (0 255 0))))
    (check assq=?
        (list->assq '((a . 1) (b . 2) (c . 3)))
        '((a . 1) (b . 2) (c . 3)))

    (check-equal? ((assq->repr-sexpr values values) '((a . 10) (b . 20) (c . 30)))
                  '(@list (cons a 10) (cons b 20) (cons c 30)))
    (check-equal? ((repr-sexpr->assq values values) '(@list (cons a 10) (cons b 20) (cons c 30)))
                  '((a . 10) (b . 20) (c . 30)))))
