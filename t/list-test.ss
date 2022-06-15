(export list-test)

(import
  :std/format :std/sugar :std/test
  ../base ../list)

(def (copy-list lst) (foldr cons '() lst))

(def list-test
  (test-suite "test suite for clan/list"
    (test-case "test extremum<-list"
      (check-equal?
       (extremum<-list < [20 12 05 08])
       5))
    (def (test-plist<->alist p a)
      (test-case (format "list plist ~s <-> alist ~s" p a)
        (check-equal? (plist<-alist a) p)
        (check-equal? (alist<-plist p) a)))
    (for-each (cut apply test-plist<->alist <>)
      '(((a: 1 b: 2 c: 3) ((a: . 1) (b: . 2) (c: . 3)))
        (() ())
        ((a (x y) b (z t)) ((a x y) (b z t)))))
    (test-case "grouping"
      (check-equal? (grouping '("abc" "b" "c" "ef" "gh" "ijk") string-length)
                    '(("abc" "ijk") ("b" "c") ("ef" "gh"))))
    (test-case "c3 linearization"
      (def my-supers
        (hash (O '())
              (A '(O)) (B '(O)) (C '(O)) (D '(O)) (E '(O))
              (K1 '(A B C)) (K2 '(D B E)) (K3 '(D A))
              (Z '(K1 K2 K3))))
      (def (my-get-supers x) (hash-get my-supers x))
      (def (my-compute-precedence-list x) (c3-compute-precedence-list x get-supers: my-get-supers))
      (check-equal? (map my-compute-precedence-list '(O A B C D E K1 K2 K3 Z))
                    '((O) (A O) (B O) (C O) (D O) (E O)
                      (K1 A B C O) (K2 D B E O) (K3 D A O)
                      (Z K1 K2 K3 D A B C E O))))
    (test-case "merge-lists"
      (check-equal? (merge-lists '(A B C D E F G) '(a b c d e) '() '() '(1 2 3 4 5 6 7 8 9))
                    '(A a 1 B b 2 C c 3 D d 4 E e 5 F 6 G 7 8 9))
      )))
