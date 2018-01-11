(export list-test)

(import
  :std/test
  :utils/base :utils/list)

(def (copy-list lst) (foldr cons '() lst))

(def list-test
  (test-suite "test suite for utils/list"
    (test-case "test extremum<-list"
      (check-equal?
       (extremum<-list < [20 12 05 08])
       5))))
