;; -*- Gerbil -*-
(export generator-test)

(import
  :std/misc/vector
  :std/sort :std/test :std/values
  ../base ../generator ../list)

(def stupid-list [1 2 3 4 5])
(def (generating-stupid-list) (generating<-list stupid-list))
(def stupid-vector #(a b c d e f))
(def (generating-stupid-vector) (generating<-vector stupid-vector))
(def stupid-list-list [[1 2 3][4 5 6][7 8 9]])
(def (generating-stupid-list-list)
  (generating<-list (map generating<-list stupid-list-list)))
(def slightly-less-stupid-list-list
  '(((1 a)(2 a)(3 a)(4 a))
    ((1 b)(3 b)(5 b)(7 b))
    ((1 c)(5 c)(6 c)(7 c))))
(def (generating-slightly-less-stupid-list-list)
  (generating<-list (map generating<-list slightly-less-stupid-list-list)))
(def (symbol<? x y) (string<? (symbol->string x) (symbol->string y)))


(def generator-test
  (test-suite "test suite for clan/generator"
    (test-case "Check contents"
      (check-equal? (list<-generating (generating-stupid-list)) stupid-list)
      (check-equal? (vector<-generating (generating-stupid-vector)) stupid-vector)
      (check-equal? (vector<-generating (generating<-vector stupid-vector start: 1 end: 4))
                    #(b c d))
      (check-equal? (vector<-generating (generating-reverse<-vector stupid-vector start: 1 end: 4))
                    #(d c b))
      (check-equal? (list<-generating
                     (generating<-for-each (λ (yield) (for-each yield stupid-list))))
                    stupid-list)
      (check-equal? (list<-generating
                     (first-value (generating<-cothread (λ (yield) (for-each yield stupid-list)))))
                    stupid-list))
    (test-case "Check count"
      (check-equal? (generating-count (generating-stupid-list)) 5))
    (test-case "Check fold"
      (check-equal? (generating-fold (generating-stupid-list) 4 +) 19))
    (test-case "Check generating-partition"
     (check-equal?
      (let-values (((even odd) (generating-partition (generating-stupid-list) even?)))
        [(list<-generating even) (list<-generating odd)])
      [[2 4][1 3 5]])
     (check-equal?
      (let-values (((a b) (generating-partition (generating-stupid-list) (λ (x) (< 1 x 4)))))
        [(list<-generating a) (list<-generating b)])
      [[2 3][1 4 5]]))
    (test-case "Check generating-merge"
      (check-equal?
       (list<-generating
        (generating-merge
         (generating-stupid-list-list)
         merge: funcall))
       [1 2 3 4 5 6 7 8 9])
      (let ((transposed
             (list<-generating
              (generating-merge
               (generating-slightly-less-stupid-list-list)
               merge: (λ (x) (sort (list<-generating x) (comparing-key test: symbol<? key: cadr)))
               priority: car))))
        (check-equal? transposed '(((1 a)(1 b)(1 c))((2 a))((3 a)(3 b))
                                   ((4 a))((5 b)(5 c))((6 c))((7 b)(7 c))))
        ;; Check that the data is transposed back into place by the same algorithm.
        (check-equal?
         (list<-generating
          (generating-merge
           (generating-slightly-less-stupid-list-list)
           merge: (λ (x) (sort (list<-generating x) (comparing-key test: < key: car)))
           priority: cadr
           priority<: symbol<?))
         slightly-less-stupid-list-list)))
    (test-case "Check generating-flatten"
     (check-equal?
      (list<-generating
       (generating-flatten (generating<-list (map generating<-list stupid-list-list))))
      [1 2 3 4 5 6 7 8 9]))
    (test-case "Check generating-singleton"
     (check-equal?
      (list<-generating (generating-singleton 'foo))
      '(foo)))
    (test-case "Check generating-concat"
     (check-equal? (list<-generating (generating-concat)) [])
     (check-equal?
      (list<-generating (generating-concat (generating<-list stupid-list)))
      stupid-list)
     (check-equal?
      (list<-generating (generating-concat
                         (generating<-list stupid-list)
                         (generating<-list stupid-list)))
      (append stupid-list stupid-list))
     (check-equal?
      (list<-generating (generating-concat
                         (generating<-list stupid-list)
                         (generating<-list stupid-list)
                         (generating<-list stupid-list)))
      (append stupid-list stupid-list stupid-list)))
    ))
