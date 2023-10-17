(export #t)

;; Trivial constructor to wrap some value. The conventional option type for our code will be
;; in pseudo-code: (deftype (option value-type) (sum-type (some value-type) '#f))
;; i.e. (some foo) represents the present of value foo, and #f represents the absence of value.
;; In Haskell you'd have option bool := Just True | Just False | None
;; In Gerbil, we'll have (option bool) := (some #t) | (some #f) | #f
(defstruct some (value) transparent: #t)

;; Options: (some value), or anything else (canonically #f) to mean absent value.
;; (deftype (Option A) (Union (Some A) '#f))
;; Note however that most (all?) of the functions below are agnostic as to what the 'absent' value is,
;; and will therefore also work for the Either / Error monad.
;;
;; (Option A) is notably isomorphic to, but different from the type (OptionList A)
;; where (OptionList A) = (Union (Pair A '()) '()) is the type of lists of A of length a most one,
;; (which is also the deterministic degenerate case of the non-determinism List monad).
;; But (Option A) is not isomorphic to either (Maybe A) or (OrFalse A) due to the treatment
;; of the exceptional value, that is uniformly quoted by Option but excluded by Maybe or OrFalse.

(def (option? x) (or (some? x) (not x))) ;; an Option is canonically (some x) or #f
(def (option-ref x) (match x ((some v) v) (else (error "no value" x))))
(def (option-get x (default #f)) (match x ((some v) v) (else default)))
(def (option-get/default x (default false)) (match x ((some v) v) (else (default))))
(def (map/option f m) (match m ((some x) (some (f x))) (else m)))
(def (list<-option x) (match x ((some v) [v]) (else [])))
(def (list-map/option f l) ;; : (Option (List B)) <- (Fun (Option B) <- A) (List A)
  (let loop ((a []) (l l))
    (match l
      ([] (some (reverse a)))
      ([x . t] (let (fx (f x))
                 (match fx
                   ((some y) (loop (cons y a) t))
                   (e e)))))))
(def (bind/option x f) (match x ((some v) (f v)) (else x)))
(def (for-each/option f x) (match x ((some v) (some (f v))) (else (void))))
