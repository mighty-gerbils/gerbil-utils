(export empty-symdict
        symdict-empty?
        symdict-ref
        symdict-put
        symdict-get
        symdict-update
        symdict-remove
        symdict-has-key?
        symdict-keys
        symdict-put/list
        list->symdict
        symdict->list
        symdict=?
        (rename: *symdict symdict))

(import :std/iter
        :std/misc/repr
        (prefix-in ./symdict-unwrapped bare-))

(defstruct symdict (unwrapped))

;; empty-symdict : [Symdictof V]
(def empty-symdict (symdict bare-empty-symdict))

;; symdict-empty? : [Symdictof V] -> Bool
(def (symdict-empty? d) (bare-symdict-empty? (symdict-unwrapped d)))

;; symdict-ref : [Symdictof V] Symbol -> V
(def (symdict-ref d k) (bare-symdict-ref (symdict-unwrapped d) k))

;; symdict-put : [Symdictof V] Symbol V -> [Symdictof V]
(def (symdict-put d k v) (symdict (bare-symdict-put (symdict-unwrapped d) k v)))

;; symdict-get : [Symdictof V] Symbol [Optional V] -> V
(def (symdict-get d k (default #f))
  (if (symdict-has-key? d k) (symdict-ref d k) default))

;; symdict-update : [Symdictof V] Symbol [V -> V] V -> [Symdictof V]
(def (symdict-update d k f v0) (symdict (bare-symdict-update (symdict-unwrapped d) k f v0)))

;; symdict-remove : [Symdictof V] Symbol -> [Symdictof V]
(def (symdict-remove d k) (symdict (bare-symdict-remove (symdict-unwrapped d) k)))

;; symdict-has-key? : [Symdictof V] Symbol -> Bool
(def (symdict-has-key? d k) (bare-symdict-has-key? (symdict-unwrapped d) k))

;; symdict-keys : [Symdictof V] -> [Listof Symbol]
(def (symdict-keys d) (bare-symdict-keys (symdict-unwrapped d)))

;; symdict-put/list : [Symdictof V] [Listof [Cons Symbol V]] -> [Symdictof V]
(def (symdict-put/list d l) (symdict (bare-symdict-put/list (symdict-unwrapped d) l)))

;; list->symdict : [Listof [Cons Symbol V]] -> [Symdictof V]
(def (list->symdict l) (symdict (bare-list->symdict l)))

;; symdict->list : [Symdictof V] -> [Listof [Cons Symbol V]]
(def (symdict->list d) (bare-symdict->list (symdict-unwrapped d)))

;; symdict=? : [Symdictof Any] [Symdictof Any] -> Bool
(def (symdict=? a b (v=? equal?))
  (bare-symdict=? (symdict-unwrapped a) (symdict-unwrapped b) v=?))

;; (*symdict (k v) ...)
;; export-renamed to (symdict (k v) ...)
(defsyntax *symdict
  (lambda (stx)
    (syntax-case stx ()
      ((_ (k v) ...)
       #'(list->symdict [(cons k v) ...])))))

;; controls print-representation, pr, prn, and repr
;; prints as (symdict (k v) ...)
(defmethod {:pr symdict}
  (lambda (self port options)
    (with ((symdict bare) self)
      (display "(symdict" port)
      (for ((k (bare-symdict-keys bare)))
        (display " (" port)
        (pr k port options)
        (display " " port)
        (pr (bare-symdict-ref bare k) port options)
        (display ")" port))
      (display ")" port))))
