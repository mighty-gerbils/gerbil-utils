(export rationaldict?
        empty-rationaldict
        rationaldict-empty?
        rationaldict-ref
        rationaldict-put
        rationaldict-update
        rationaldict-remove
        rationaldict-has-key?
        rationaldict-keys
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

;; rationaldict-ref : [Rationaldictof V] Int -> V
(def (rationaldict-ref d k) (bare-rationaldict-ref (rationaldict-unwrapped d) k))

;; rationaldict-put : [Rationaldictof V] Int V -> [Rationaldictof V]
(def (rationaldict-put d k v) (rationaldict (bare-rationaldict-put (rationaldict-unwrapped d) k v)))

;; rationaldict-update : [Rationaldictof V] Int [V -> V] V -> [Rationaldictof V]
(def (rationaldict-update d k f v0) (rationaldict (bare-rationaldict-update (rationaldict-unwrapped d) k f v0)))

;; rationaldict-remove : [Rationaldictof V] Int -> [Rationaldictof V]
(def (rationaldict-remove d k) (rationaldict (bare-rationaldict-remove (rationaldict-unwrapped d) k)))

;; rationaldict-has-key? : [Rationaldictof V] Int -> Bool
(def (rationaldict-has-key? d k) (bare-rationaldict-has-key? (rationaldict-unwrapped d) k))

;; rationaldict-keys : [Rationaldictof V] -> [Listof Int]
(def (rationaldict-keys d) (bare-rationaldict-keys (rationaldict-unwrapped d)))

;; rationaldict-put/list : [Rationaldictof V] [Listof [Cons Int V]] -> [Rationaldictof V]
(def (rationaldict-put/list d l) (rationaldict (bare-rationaldict-put/list (rationaldict-unwrapped d) l)))

;; list->rationaldict : [Listof [Cons Int V]] -> [Rationaldictof V]
(def (list->rationaldict l) (rationaldict (bare-list->rationaldict l)))

;; rationaldict->list : [Rationaldictof V] -> [Listof [Cons Int V]]
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

;; rationaldict-min-key : [Rationaldictof V] ?X -> [Or Int X]
(def (rationaldict-min-key a (default #f))
  (bare-rationaldict-min-key (rationaldict-unwrapped a) default))

;; rationaldict-max-key : [Rationaldictof V] ?X -> [Or Int X]
(def (rationaldict-max-key a (default #f))
  (bare-rationaldict-max-key (rationaldict-unwrapped a) default))
