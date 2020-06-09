;;; Reified failures
(export #t)
(import :std/sugar ./option)

(defstruct failure (error) transparent: #t)

;; A Result is (some x) or (failure e)
;; (deftype (Result V E) (Or (Some V) (Failure E)))
;; : Bool <- Any
(def (result? x) (or (some? x) (failure? x)))

;; : (Result A Err) <- (A <- Unit)
(def (call/result thunk) (with-catch make-failure (some (thunk))))

(defrule (with-result body ...) (call/result (lambda () body ...)))
