(export symdict-test)

(import :std/test
        :clan/pure/dict/symdict
        :clan/pure/dict/assq)

(def symdict-test
  (test-suite "test suite for clan/pure/dict/symdict"
    (check symdict=? (list->symdict []) empty-symdict)
    (check-equal? (symdict->list empty-symdict) [])
    (check-equal? (symdict-empty? empty-symdict) #t)
    (check-equal? (symdict-empty? (list->symdict '((red . 5)))) #f)
    (check-equal? (symdict->list (list->symdict '((red . 5)))) '((red . 5)))

    (check-equal? (symdict-ref (list->symdict '((red . 5) (blue . 4) (black . 1))) 'blue)
                  4)
    (check-equal? (symdict-ref (list->symdict '((red . 5) (blue . 4) (black . 1))) 'red)
                  5)
    (check-equal? (symdict-ref (list->symdict '((red . 5) (blue . 4) (black . 1))) 'black)
                  1)

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
           (symdict-update (list->symdict '((a . 10) (b . 20) (c . 30))) 'b 1+ 0)
           (list->symdict '((b . 21) (a . 10) (c . 30))))
    (check symdict=?
           (symdict-update (list->symdict '((a . 10) (b . 20) (c . 30))) 'd 1+ 0)
           (list->symdict '((d . 1) (a . 10) (b . 20) (c . 30))))

    (check symdict=?
           (symdict-remove (list->symdict '((a . 10) (b . 20) (c . 30))) 'b)
           (list->symdict '((a . 10) (c . 30))))
    (check symdict=?
           (symdict-remove (list->symdict '((a . 10) (b . 20) (c . 30))) 'd)
           (list->symdict '((a . 10) (b . 20) (c . 30))))

    (check-equal? (symdict-has-key? (list->symdict '((a . 10) (b . 20) (c . 30))) 'a) #t)
    (check-equal? (symdict-has-key? (list->symdict '((a . 10) (b . 20) (c . 30))) 'b) #t)
    (check-equal? (symdict-has-key? (list->symdict '((a . 10) (b . 20) (c . 30))) 'c) #t)
    (check-equal? (symdict-has-key? (list->symdict '((a . 10) (b . 20) (c . 30))) 'd) #f)

    (def (list-set=? as bs)
      (and (= (length as) (length bs))
           (andmap (lambda (a) (member a bs)) as)
           (andmap (lambda (b) (member b as)) bs)))

    (check list-set=?
           (symdict-keys (list->symdict '((red . 5) (blue . 4) (black . 1))))
           ['red 'blue 'black])

    (check symdict=?
           (list->symdict '((blue . (0 255 0)) (red . (255 0 0))))
           (list->symdict '((red . (255 0 0)) (blue . (0 255 0)))))
    (check assq=?
           (symdict->list (list->symdict '((a . 1) (b . 2) (c . 3))))
           '((a . 1) (b . 2) (c . 3)))))
