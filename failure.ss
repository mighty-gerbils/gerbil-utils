;;; Reified failures.
;; TODO: rename this file result? Split into failure and result ?
(export #t)
(import
  :std/error
  :std/sugar
  ./source
  ./option)

(defclass (Failure Exception) (error) transparent: #t)
(def failure? Failure?)
(def (failure_ e) (Failure error: e))

(defsyntax-for-match failure
  (syntax-rules () ((_ e) (Failure error: e)) ((_) (Failure)) )
  (syntax-rules () ((_ e) (Failure error: e))
                ((ctx . a) (syntax-error "failure takes one argument" (ctx . a)))
                (_ failure_)))

;; A Result is (some x) or (failure e)
;; (deftype (Result V E) (Or (Some V) (Failure E)))
;; : Bool <- Any
(def (result? x) (or (some? x) (failure? x)))

;; : (Result A Err) <- (A <- Unit)
(def (call/result thunk)
  (with-catch failure (cut some (thunk))))

(defrule (with-result body ...) (call/result (lambda () body ...)))

(def (run-result r)
  (match r
    ((some r) r)
    ((failure f) (raise f))
    (#f (raise (failure #f)))))
