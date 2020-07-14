(export list-test)

(import
  :std/format :std/test
  ../base ../list)

(def (copy-list lst) (foldr cons '() lst))

(def list-test
  (test-suite "test suite for utils/list"
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
        ((a (x y) b (z t)) ((a x y) (b z t)))))))
