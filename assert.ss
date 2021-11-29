;; -*- Gerbil -*-
;;;; Assertions

(export #t)

(import
  (for-syntax :gerbil/expander)
  :std/logger :std/sugar :std/format
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

(defrules assert!/where ()
  ((_ condition message (name expr) ...)
   (let ((name expr) ...)
     (assert!/where-helper condition message 'condition [(cons 'name name) ...]))))

(begin-syntax
  ;; original idea from Jack Firth, Sam Phillips, and Alex Knauth for Rackunit:
  ;; https://github.com/racket/rackunit/issues/149#issuecomment-919208710
  ;; special-identifier? : Any -> Bool
  (def (special-identifier? stx)
    (and (identifier? stx)
         (or (core-bound-identifier? stx)
             (and (syntax-local-value stx false) #t))))

  ;; split-sub-exprs : Stx -> [Stx [[Id Stx] ...]]
  (def (split-sub-exprs stx)
    (syntax-case stx ()
      ((f a ...)
       (not (special-identifier? #'f))
       (with-syntax (((x ...) (gentemps #'(a ...))))
         [(syntax/loc stx (f x ...)) #'((x a) ...)]))
      (_ [stx []])))

  ;; srcloc-string : Stx -> String
  (def (srcloc-string stx)
    (def loc (stx-source stx))
    (cond (loc (call-with-output-string "" (cut ##display-locat loc #t <>)))
          (else "?"))))

(defsyntax assert!!
  (lambda (stx)
    (syntax-case stx ()
      ((_ condition)
       (with-syntax (((c ((x e) ...)) (split-sub-exprs #'condition))
                     (message (srcloc-string #'condition)))
         #'(let ((x e) ...)
             (assert!/where-helper c 'message 'condition [(cons 'e x) ...]))))
      ((_ condition message)
       (with-syntax (((c ((x e) ...)) (split-sub-exprs #'condition)))
         #'(let ((x e) ...)
             (assert!/where-helper c message 'condition [(cons 'e x) ...]))))
      ((_ condition message expr ...)
       #'(assert!/where-helper condition message 'condition [(cons 'expr expr) ...])))))

(def (assert!/where-helper condition message condition-expr extras)
  (unless condition
   (let ()
    (def hd (format "Assertion failed ~a: ~s" message condition-expr))
    (def str (apply string-append hd (map (match <> ((cons k v) (format "\n  ~s => ~r" k v))) extras)))
    (error str))))
