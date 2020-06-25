(export assq-test)

(import :std/test
        ../assq)

(def assq-test
  (test-suite "test suite for clan/pure/dict/assq"
    (check-equal? (list->assq []) [])
    (check-equal? (list->assq '((red . 5))) '((red . 5)))

    (check-equal? (assq-ref '((red . 5) (blue . 4) (black . 1)) 'blue)
                4)
    (check-equal? (assq-ref '((red . 5) (blue . 4) (black . 1)) 'red)
                5)
    (check-equal? (assq-ref '((red . 5) (blue . 4) (black . 1)) 'black)
                1)

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

    (check assq=?
        '((blue . (0 255 0)) (red . (255 0 0)))
        '((red . (255 0 0)) (blue . (0 255 0))))
    (check assq=?
        (list->assq '((a . 1) (b . 2) (c . 3)))
        '((a . 1) (b . 2) (c . 3)))))
