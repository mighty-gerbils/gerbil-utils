(export empty-dicteq
        dicteq-empty?
        dicteq-ref
        dicteq-put
        dicteq-update
        dicteq-remove
        dicteq-has-key?
        dicteq-keys
        dicteq-put/list
        list->dicteq
        dicteq->list
        dicteq=?
        (rename: *dicteq dicteq))

(import :std/iter
        :std/misc/repr
        (prefix-in ./dicteq-unwrapped bare-))

(defstruct dicteq (unwrapped))

;; empty-dicteq : [DictEqof K V]
(def empty-dicteq (dicteq bare-empty-dicteq))

;; dicteq-empty? : [DictEqof K V] -> Bool
(def (dicteq-empty? d) (bare-dicteq-empty? (dicteq-unwrapped d)))

;; dicteq-ref : [DictEqof K V] K -> V
(def (dicteq-ref d k) (bare-dicteq-ref (dicteq-unwrapped d) k))

;; dicteq-put : [DictEqof K V] K V -> [DictEqof K V]
(def (dicteq-put d k v) (dicteq (bare-dicteq-put (dicteq-unwrapped d) k v)))

;; dicteq-update : [DictEqof K V] K [V -> V] V -> [DictEqof K V]
(def (dicteq-update d k f v0) (dicteq (bare-dicteq-update (dicteq-unwrapped d) k f v0)))

;; dicteq-remove : [DictEqof K V] K -> [DictEqof K V]
(def (dicteq-remove d k) (dicteq (bare-dicteq-remove (dicteq-unwrapped d) k)))

;; dicteq-has-key? : [DictEqof K V] K -> Bool
(def (dicteq-has-key? d k) (bare-dicteq-has-key? (dicteq-unwrapped d) k))

;; dicteq-keys : [DictEqof K V] -> [Listof K]
(def (dicteq-keys d) (bare-dicteq-keys (dicteq-unwrapped d)))

;; dicteq-put/list : [DictEqof K V] [Listof [Cons K V]] -> [DictEqof K V]
(def (dicteq-put/list d l) (dicteq (bare-dicteq-put/list (dicteq-unwrapped d) l)))

;; list->dicteq : [Listof [Cons K V]] -> [DictEqof K V]
(def (list->dicteq l) (dicteq (bare-list->dicteq l)))

;; dicteq->list : [DictEqof K V] -> [Listof [Cons K V]]
(def (dicteq->list d) (bare-dicteq->list (dicteq-unwrapped d)))

;; dicteq=? : [DictEqof Any Any] [DictEqof Any Any] -> Bool
(def (dicteq=? a b (v=? equal?))
  (bare-dicteq=? (dicteq-unwrapped a) (dicteq-unwrapped b) v=?))

;; (*dicteq (k v) ...)
;; export-renamed to (dicteq (k v) ...)
(defsyntax *dicteq
  (lambda (stx)
    (syntax-case stx ()
      ((_ (k v) ...)
       #'(list->dicteq [(cons k v) ...])))))

;; controls print-representation, pr, prn, and repr
;; prints as (dicteq (k v) ...)
(defmethod {:pr dicteq}
  (lambda (self port options)
    (with ((dicteq bare) self)
      (display "(dicteq" port)
      (for ((k (bare-dicteq-keys bare)))
        (display " (" port)
        (pr k port options)
        (display " " port)
        (pr (bare-dicteq-ref bare k) port options)
        (display ")" port))
      (display ")" port))))
