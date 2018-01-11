(export base-test)

(import
  :std/test
  :utils/base)

(def (double x) (* x 2))
(def (square x) (* x x))
(def octosquare (compose double square double)) ;; x |-> 8*x^2
(def (copy-list lst) (foldr cons '() lst))

(def base-test
  (test-suite "test suite for utils/base"
    (test-case "test compose operator"
      (check-eqv? (octosquare 1) 8)
      (check-eqv? (octosquare 2) 32)
      (check-eqv? (octosquare 3) 72))

    (test-case "test pipeline operator"
      (check-eqv? (double (square 3)) (!> 3 square double))
      (check-eqv? (double (square 3)) 18)
      (check-eqv? (!> 3 square double) 18))

    (test-case "test ignore-errors"
      (check-equal? (ignore-errors (+ 1 1)) 2)
      (check-equal? (ignore-errors #f) #f)
      (check-equal? (ignore-errors (error "foo")) #f))

    (test-case "test shift!"
      (let ((a 1) (b 2) (c 3) (d 4))
        (shift! a b c d)
        (check-equal? [a b c d] [2 3 4 4])))

    (test-case "test rotate!"
      (let ((a 1) (b 2) (c 3) (d 4))
        (rotate! a b c d)
        (check-equal? [a b c d] [2 3 4 1])))))
