;;; Reified failures.
;; TODO: rename this file result? Split into failure and result ?
(export #t)
(import
  :std/error
  :std/sugar
  ./option)

(defclass (Failure Exception) (error) transparent: #t)
(def failure? Failure?)

(defsyntax-for-match failure
  (lambda (stx)
    (syntax-case stx () ; match pattern
      ((_) #'(Failure))
      ((_ e) #'(Failure error: e))))
  (lambda (stx)
    (syntax-case stx () ; expr
      ((_ e)
       #'(Failure error: e))
      ((_ . arg)
       (error "failure takes one argument"))
      (_ (lambda (e) #'(Failure error: e))))))

;; A Result is (some x) or (failure e)
;; (deftype (Result V E) (Or (Some V) (Failure E)))
;; : Bool <- Any
(def (result? x) (or (some? x) (Failure? x)))

;; : (Result A Err) <- (A <- Unit)
(def (call/result thunk)
  (with-catch (cut Failure error: <>) (cut some (thunk))))

(defrule (with-result body ...) (call/result (lambda () body ...)))
