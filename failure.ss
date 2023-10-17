;;; Reified failures.
;; TODO: rename this file result? Split into failure and result ?
(export (rename: failure_ failure) failure? Failure Failure?
        result? call/result with-result run-result)

(import
  :std/error
  :std/sugar
  ./source
  ./option)

(defclass (Failure Exception) (error) transparent: #t)
(def failure? Failure?)
(def (failure e) (Failure error: e))

(defsyntax-for-match failure_
  (syntax-rules () ((_ e) (Failure error: e)) ((_) (Failure)) )
  (syntax-rules () ((_ e) (Failure error: e))
                ((ctx . a) (syntax-error "failure takes one argument" (ctx . a)))
                (_ failure)))

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
    ((failure_ f) (raise f))
    (#f (raise (failure #f)))))
