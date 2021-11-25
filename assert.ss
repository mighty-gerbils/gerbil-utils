;; -*- Gerbil -*-
;;;; Assertions

(export #t)

(import
  :std/logger :std/sugar
  ./base ./error)

(defrules assert-comparison! ()
  ((_ pred expr1 expr2) (assert-comparison-helper! 'pred 'expr1 'expr2 pred expr1 expr2)))

(defrules assert-equal! ()
  ((_ expr1 expr2) (assert-comparison! equal? expr1 expr2)))

(def (assert-comparison-helper! pred-name expr1 expr2 pred val1 val2)
  (unless (pred val1 val2)
    (warn-and-err "Comparison failed: (~s ~s ~s)\n first value was: ~s\n second value was: ~s\n"
             pred-name expr1 expr2 val1 val2)
    (error "Comparison failed" pred-name expr1 expr2 val1 val2)))

;; --------------------------------------------------------

(defrules assert!/where
  ((_ condition message (name expr) ...)
   (let ((name expr) ...)
     (assert!/where-helper condition message 'condition [(cons 'name name) ...]))))

(defrules assert! ()
  ((_ condition message expr ...)
   (assert!/where-helper condition message 'condition [(cons 'expr expr) ...]))
  ((_ condition message)
   (unless expr
     (error "Assertion failed" message 'expr)))
  ((_ condition)
   (unless expr
     (error "Assertion failed" 'expr))))

(def (assert!/where-helper condition message condition-expr extras)
  (unless condition
    (def hd (format "Assertion failed ~a: ~s" message condition-expr))
    (def str (apply string-append hd (map (match <> ((cons k v) (format "\n  ~s => ~r" k v))) extras)))
    (error str)))
