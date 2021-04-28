(export rationaldict?
        empty-rationaldict
        rationaldict-empty?
        rationaldict-ref
        rationaldict-get
        rationaldict-put
        rationaldict-update
        rationaldict-remove
        rationaldict-has-key?
        rationaldict-keys
        rationaldict-values
        rationaldict-put/list
        list->rationaldict
        rationaldict->list
        rationaldict=?
        (rename: *rationaldict rationaldict)
        rationaldict-min-key
        rationaldict-max-key)

(import :std/iter
        :std/misc/repr
        (prefix-in ./rationaldict-unwrapped bare-))

(defstruct rationaldict (unwrapped))

;; empty-rationaldict : [Rationaldictof V]
(def empty-rationaldict (rationaldict bare-empty-rationaldict))

;; rationaldict-empty? : [Rationaldictof V] -> Bool
(def (rationaldict-empty? d) (bare-rationaldict-empty? (rationaldict-unwrapped d)))

;; rationaldict-ref : [Rationaldictof V] Rational [-> V] -> V
(def (rationaldict-ref d k (default (cut error "rationaldict-ref: No value associated with key" d k)))
  (bare-rationaldict-ref (rationaldict-unwrapped d) k default))

;; rationaldict-get : [Rationaldictof V] Rational ?V -> V
(def (rationaldict-get d k (default #f))
  (bare-rationaldict-get (rationaldict-unwrapped d) k default))

;; rationaldict-put : [Rationaldictof V] Rational V -> [Rationaldictof V]
(def (rationaldict-put d k v) (rationaldict (bare-rationaldict-put (rationaldict-unwrapped d) k v)))

;; rationaldict-update : [Rationaldictof V] Rational [V -> V] V -> [Rationaldictof V]
(def (rationaldict-update d k f v0) (rationaldict (bare-rationaldict-update (rationaldict-unwrapped d) k f v0)))

;; rationaldict-remove : [Rationaldictof V] Rational -> [Rationaldictof V]
(def (rationaldict-remove d k) (rationaldict (bare-rationaldict-remove (rationaldict-unwrapped d) k)))

;; rationaldict-has-key? : [Rationaldictof V] Rational -> Bool
(def (rationaldict-has-key? d k) (bare-rationaldict-has-key? (rationaldict-unwrapped d) k))

;; rationaldict-keys : [Rationaldictof V] -> [Listof Rational]
(def (rationaldict-keys d) (bare-rationaldict-keys (rationaldict-unwrapped d)))

;; rationaldict-values : [Rationaldictof V] -> [Listof V]
(def (rationaldict-values d) (bare-rationaldict-values (rationaldict-unwrapped d)))

;; rationaldict-put/list : [Rationaldictof V] [Listof [Cons Rational V]] -> [Rationaldictof V]
(def (rationaldict-put/list d l) (rationaldict (bare-rationaldict-put/list (rationaldict-unwrapped d) l)))

;; list->rationaldict : [Listof [Cons Rational V]] -> [Rationaldictof V]
(def (list->rationaldict l) (rationaldict (bare-list->rationaldict l)))

;; rationaldict->list : [Rationaldictof V] -> [Listof [Cons Rational V]]
(def (rationaldict->list d) (bare-rationaldict->list (rationaldict-unwrapped d)))

;; rationaldict=? : [Rationaldictof Any] [Rationaldictof Any] -> Bool
(def (rationaldict=? a b (v=? equal?))
  (bare-rationaldict=? (rationaldict-unwrapped a) (rationaldict-unwrapped b) v=?))

;; (*rationaldict (k v) ...)
;; export-renamed to (rationaldict (k v) ...)
(defsyntax *rationaldict
  (lambda (stx)
    (syntax-case stx ()
      ((_ (k v) ...)
       #'(list->rationaldict [(cons k v) ...])))))

;; controls print-representation, pr, prn, and repr
;; prints as (rationaldict (k v) ...)
(defmethod {:pr rationaldict}
  (lambda (self port options)
    (with ((rationaldict bare) self)
      (display "(rationaldict" port)
      (for ((k (bare-rationaldict-keys bare)))
        (display " (" port)
        (pr k port options)
        (display " " port)
        (pr (bare-rationaldict-ref bare k) port options)
        (display ")" port))
      (display ")" port))))

;; rationaldict-min-key : [Rationaldictof V] ?X -> [Or Rational X]
(def (rationaldict-min-key a (default #f))
  (bare-rationaldict-min-key (rationaldict-unwrapped a) default))

;; rationaldict-max-key : [Rationaldictof V] ?X -> [Or Rational X]
(def (rationaldict-max-key a (default #f))
  (bare-rationaldict-max-key (rationaldict-unwrapped a) default))
