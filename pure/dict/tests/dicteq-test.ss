(export dicteq-test)

(import :std/test
        :clan/pure/dict/dicteq
        :clan/pure/dict/assq)

(def dicteq-test
  (test-suite "test suite for clan/pure/dict/dicteq"
    (check-equal? (list->dicteq []) empty-dicteq)
    (check-equal? (dicteq->list empty-dicteq) [])
    (check-equal? (dicteq-empty? empty-dicteq) #t)
    (check-equal? (dicteq-empty? (list->dicteq '((red . 5)))) #f)
    (check-equal? (dicteq->list (list->dicteq '((red . 5)))) '((red . 5)))

    (check-equal? (dicteq-ref (list->dicteq '((red . 5) (blue . 4) (black . 1))) 'blue)
                  4)
    (check-equal? (dicteq-ref (list->dicteq '((red . 5) (blue . 4) (black . 1))) 'red)
                  5)
    (check-equal? (dicteq-ref (list->dicteq '((red . 5) (blue . 4) (black . 1))) 'black)
                  1)

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
           (dicteq-update (list->dicteq '((a . 10) (b . 20) (c . 30))) 'b 1+ 0)
           (list->dicteq '((b . 21) (a . 10) (c . 30))))
    (check dicteq=?
           (dicteq-update (list->dicteq '((a . 10) (b . 20) (c . 30))) 'd 1+ 0)
           (list->dicteq '((d . 1) (a . 10) (b . 20) (c . 30))))

    (check dicteq=?
           (dicteq-remove (list->dicteq '((a . 10) (b . 20) (c . 30))) 'b)
           (list->dicteq '((a . 10) (c . 30))))
    (check dicteq=?
           (dicteq-remove (list->dicteq '((a . 10) (b . 20) (c . 30))) 'd)
           (list->dicteq '((a . 10) (b . 20) (c . 30))))

    (check-equal? (dicteq-has-key? (list->dicteq '((a . 10) (b . 20) (c . 30))) 'a) #t)
    (check-equal? (dicteq-has-key? (list->dicteq '((a . 10) (b . 20) (c . 30))) 'b) #t)
    (check-equal? (dicteq-has-key? (list->dicteq '((a . 10) (b . 20) (c . 30))) 'c) #t)
    (check-equal? (dicteq-has-key? (list->dicteq '((a . 10) (b . 20) (c . 30))) 'd) #f)

    (def (list-set=? as bs)
      (and (andmap (lambda (a) (member a bs)) as)
           (andmap (lambda (b) (member b as)) bs)))

    (check list-set=?
           (dicteq-keys (list->dicteq '((red . 5) (blue . 4) (black . 1))))
           ['red 'blue 'black])

    (check dicteq=?
           (list->dicteq '((blue . (0 255 0)) (red . (255 0 0))))
           (list->dicteq '((red . (255 0 0)) (blue . (0 255 0)))))
    (check assq=?
           (dicteq->list (list->dicteq '((a . 1) (b . 2) (c . 3))))
           '((a . 1) (b . 2) (c . 3)))))
