(export stateful-avl-map-test)
(export #t)

(import
  :std/iter :std/misc/list :std/sort :std/srfi/1 :std/test
  ../base ../generator ../list ../number ../random ../roman ../stateful-avl-map)

;; NB: these functions assume no repeated / shadowed keys in alist-s, which is the case in these tests.
(def N number-comparer)
(def (sort-alist alist) (sort alist (comparing-key test: < key: car)))
(def (make-alist n (fun number->string))
  (for/collect (i (in-iota n 1)) (cons i (fun i))))
(def (alist-equal? x y) (equal? (sort-alist x) (sort-alist y)))
(def (alist<- l) (alist<-avl-map l))
(def (<-alist l) (avl-map<-alist N l))

(def *alist-10-decimal* (make-alist 10 number->string))
(def *alist-10-roman* (make-alist 10 roman-numeral<-integer))
(def *alist-100-decimal* (make-alist 100 number->string))
(def *alist-100-roman* (make-alist 100 roman-numeral<-integer))

(def *al-1* (shuffle-list *alist-100-decimal*))
(def *al-2* (filter (compose even? car) *alist-100-decimal*))
(def *al-3* (filter (λ (x) (< (string-length (cdr x)) 5)) *alist-100-roman*))
(def *al-5* (delete-duplicates (append *al-2* *al-3*) (comparing-key key: car)))

(def stateful-avl-map-test
  (test-suite "test suite for clan/stateful-avl-map"

    (test-case "empty"
      (check-predicate (alist<- (make-empty-avl-map)) null?)
      (check-predicate (<-alist '()) avl-map-empty?))

    (test-case "lookup"
      (check-equal?
       (values->list
        (avl-map-lookup N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 12))
       '("12" #t))
      (check-equal?
       (values->list
        (avl-map-lookup N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 13))
       '(#f #f))
      (let ((m (<-alist *al-1*)))
        (for-each! *al-1*
                   (λ-match ([k . v]
                             (check-equal? (values->list (avl-map-lookup N m k))
                                           [v #t]))))))

    (test-case "ref"
      (check-equal?
       (avl-map-ref N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 12)
       "12")
      (check-equal?
       (avl-map-ref N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 12 "BLAH")
       "12")
      (check-exception
       (avl-map-ref N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 13)
       true)
      (check-equal?
       (avl-map-ref N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 13 "BLAH")
       "BLAH")
      (let ((m (<-alist *al-1*)))
        (for-each! *al-1*
                   (λ-match ([k . v] (check-equal? v (avl-map-ref N m k)))))))

    (test-case "get"
      (check-equal?
       (avl-map-get N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 12)
       "12")
      (check-equal?
       (avl-map-get N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 13)
       #f)
      (let ((m (<-alist *al-1*)))
        (for-each! *al-1*
                   (λ-match ([k . v] (check-equal? v (avl-map-get N m k)))))))

    (test-case "key?"
      (check-equal?
       (avl-map-key? N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 12)
       #t)
      (check-equal?
       (avl-map-key? N (<-alist '((57 . "57") (10 . "10") (12 . "12"))) 13)
       #f)
      (let ((m (<-alist *al-1*)))
        (for-each! *al-1*
                   (λ-match ([k . v] (check-equal? (avl-map-key? N m k) #t))))))

    ;; NB: avl-map naturally preserve order, so no sort-alist.
    (test-case "<-alist and back"
      (check-equal? *alist-10-decimal* (alist<- (<-alist *alist-10-decimal*)))
      (check-equal? *alist-100-roman* (alist<- (<-alist *alist-100-roman*)))
      (check-equal? *alist-100-decimal* (alist<- (<-alist *al-1*))))

    (test-case "key-value"
      (check-equal? (values->list (avl-map-leftmost (<-alist '((42 . "x"))))) '(42 "x")))

    (test-case "leftmost"
      (check-equal? (values->list (avl-map-leftmost (<-alist *alist-10-decimal*))) '(1 "1"))
      (check-equal? (values->list (avl-map-leftmost (<-alist *alist-100-roman*))) '(1 "I")))

    (test-case "rightmost"
      (check-equal? (values->list (avl-map-rightmost (<-alist *alist-10-decimal*))) '(10 "10"))
      (check-equal? (values->list (avl-map-rightmost (<-alist *alist-100-roman*))) '(100 "C")))

    (test-case "size"
      (check-equal? (avl-map-size (<-alist *alist-10-decimal*)) 10)
      (check-equal? (avl-map-size (<-alist *alist-100-roman*)) 100))

    (test-case "for-each!"
      (check-equal?
       (with-list-builder (c!) (avl-map-for-each! (<-alist *alist-10-roman*) c!))
       *alist-10-roman*))

    (test-case "put!"
      (check-equal? (alist<- (with-avl-map (m) (avl-map-put! N m 0 "0")))
                    '((0 . "0")))
      (check-equal? (alist<- (with-avl-map (m '((1 . "1") (3 . "3"))) (avl-map-put! N m 2 "2")))
                    (make-alist 3)))

    (test-case "put! and size"
      (with-avl-map (m *al-1*)
        (avl-map-put! N m 100 "one hundred")
        (check-equal? (avl-map-size m) 100)
        (avl-map-put! N m 101 "101")
        (check-equal? (avl-map-size m) 101)))

    (test-case "remove!"
      (with-avl-map (m) (check-equal? (avl-map-remove! N m 0 42) 42))
      (with-avl-map (m '((1 . "1") (2 . "2")))
        (check-equal? (avl-map-remove! N m 1) "1")
        (check-equal? (avl-map-remove! N m 2 42) "2")
        (check-equal? (avl-map-remove! N m 2 42) 42))
      (with-avl-map (m *al-1*)
        (check-equal? (avl-map-size m) 100)
        (check-equal? (avl-map-remove! N m 42) "42")
        (check-equal? (avl-map-size m) 99)
        (avl-map-put! N m 42 "42")
        (let loop ((l *al-1*))
          (match l
            ([[k . v] . r]
             (check-equal? (avl-map-remove! N m k) v)
             (loop r))
            (_
             (check-predicate m avl-map-empty?))))))

    (test-case "generating"
      (with-avl-map (m *al-1*)
        (check-equal? (list<-generating (generating<-avl-map m))
                      *alist-100-decimal*)
        (check-equal? (list<-generating (generating-reverse<-avl-map m))
                      (reverse *alist-100-decimal*))))

    (test-case "take"
      (with-avl-map (m *al-1*)
        (check-equal? (avl-map-take-left m 10) *alist-10-decimal*)
        (check-equal? (avl-map-take-right m 5)
                      '((96 . "96") (97 . "97") (98 . "98") (99 . "99") (100 . "100"))))
      (check-equal? (avl-map-take-left (<-alist *alist-10-roman*) 20 #t) *alist-10-roman*)
      (check-exception (avl-map-take-left (<-alist *alist-10-roman*) 20) true)
      (check-equal? (avl-map-take-right (<-alist *alist-10-roman*) 20 #t) *alist-10-roman*)
      (check-exception (avl-map-take-right (<-alist *alist-10-roman*) 20) true))
))
