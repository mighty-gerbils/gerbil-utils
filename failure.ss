;;; Reified failures.
;; TODO: rename this file result? Split into failure and result ?
(export #t)
(import
  :std/error
  :std/sugar
  ./option)

(defclass (failure Exception) (error) transparent: #t)

;; A Result is (some x) or (failure e)
;; (deftype (Result V E) (Or (Some V) (Failure E)))
;; : Bool <- Any
(def (result? x) (or (some? x) (failure? x)))

;; : (Result A Err) <- (A <- Unit)
(def (call/result thunk)
  (with-catch (cut make-failure error: <>) (cut some (thunk))))

(defrule (with-result body ...) (call/result (lambda () body ...)))
