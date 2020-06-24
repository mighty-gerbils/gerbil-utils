(export intdict?
        empty-intdict
        intdict-empty?
        intdict-ref
        intdict-get
        intdict-put
        intdict-update
        intdict-remove
        intdict-has-key?
        intdict-keys
        intdict-put/list
        list->intdict
        intdict->list
        intdict=?
        (rename: *intdict intdict))

(import :std/iter
        :std/misc/repr
        (prefix-in :clan/pure/dict/intdict-unwrapped bare-))

(defstruct intdict (unwrapped))

;; empty-intdict : [Intdictof V]
(def empty-intdict (intdict bare-empty-intdict))

;; intdict-empty? : [Intdictof V] -> Bool
(def (intdict-empty? d) (bare-intdict-empty? (intdict-unwrapped d)))

;; intdict-ref : [Intdictof V] Int -> V
(def (intdict-ref d k) (bare-intdict-ref (intdict-unwrapped d) k))

;; intdict-get : [Intdictof V] Int F -> (U V F)
(def (intdict-get d k (default #f)) (bare-intdict-get (intdict-unwrapped d) k default))

;; intdict-put : [Intdictof V] Int V -> [Intdictof V]
(def (intdict-put d k v) (intdict (bare-intdict-put (intdict-unwrapped d) k v)))

;; intdict-update : [Intdictof V] Int [V -> V] V -> [Intdictof V]
(def (intdict-update d k f v0) (intdict (bare-intdict-update (intdict-unwrapped d) k f v0)))

;; intdict-remove : [Intdictof V] Int -> [Intdictof V]
(def (intdict-remove d k) (intdict (bare-intdict-remove (intdict-unwrapped d) k)))

;; intdict-has-key? : [Intdictof V] Int -> Bool
(def (intdict-has-key? d k) (bare-intdict-has-key? (intdict-unwrapped d) k))

;; intdict-keys : [Intdictof V] -> [Listof Int]
(def (intdict-keys d) (bare-intdict-keys (intdict-unwrapped d)))

;; intdict-put/list : [Intdictof V] [Listof [Cons Int V]] -> [Intdictof V]
(def (intdict-put/list d l) (intdict (bare-intdict-put/list (intdict-unwrapped d) l)))

;; list->intdict : [Listof [Cons Int V]] -> [Intdictof V]
(def (list->intdict l) (intdict (bare-list->intdict l)))

;; intdict->list : [Intdictof V] -> [Listof [Cons Int V]]
(def (intdict->list d) (bare-intdict->list (intdict-unwrapped d)))

;; intdict=? : [Intdictof Any] [Intdictof Any] -> Bool
(def (intdict=? a b (v=? equal?))
  (bare-intdict=? (intdict-unwrapped a) (intdict-unwrapped b) v=?))

;; (*intdict (k v) ...)
;; export-renamed to (intdict (k v) ...)
(defsyntax *intdict
  (lambda (stx)
    (syntax-case stx ()
      ((_ (k v) ...)
       #'(list->intdict [(cons k v) ...])))))

;; controls print-representation, pr, prn, and repr
;; prints as (intdict (k v) ...)
(defmethod {:pr intdict}
  (lambda (self port options)
    (with ((intdict bare) self)
      (display "(intdict" port)
      (for ((k (bare-intdict-keys bare)))
        (display " (" port)
        (pr k port options)
        (display " " port)
        (pr (bare-intdict-ref bare k) port options)
        (display ")" port))
      (display ")" port))))
