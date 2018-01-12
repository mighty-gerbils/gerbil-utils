;; -*- Gerbil -*-
;;;; Assertions

(export #t)

(import
  :std/logger :std/sugar
  :clan/utils/base :clan/utils/error)

(defrules assert-comparison! ()
  ((_ pred expr1 expr2) (assert-comparison-helper! 'pred 'expr1 'expr2 pred expr1 expr2)))

(defrules assert-equal! ()
  ((_ expr1 expr2) (assert-comparison! equal? expr1 expr2)))

(def (assert-comparison-helper! pred-name expr1 expr2 pred val1 val2)
  (unless (pred val1 val2)
    (warn-and-err "Comparison failed: (~s ~s ~s)\n first value was: ~s\n second value was: ~s\n"
             pred-name expr1 expr2 val1 val2)
    (error "Comparison failed" pred-name expr1 expr2 val1 val2)))
