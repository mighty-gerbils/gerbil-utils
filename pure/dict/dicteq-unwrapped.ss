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
        dicteq=?)

(import :std/iter
        :std/misc/rbtree
        :std/srfi/1
        ./assq
        ./intdict-unwrapped)



;; Functional Dictionaries mapping keys to values according to eq-ness

;; An [Assqof K V] is a [Listof [Cons K V]]
;; where the keys should be compared by `eq?`,
;; and there are no duplicate keys

;; A [DictEqof K V] is an [Intdictof [Assqof K V]]

;; empty-dicteq : [DictEqof K V]
(def empty-dicteq empty-intdict)

;; dicteq-empty? : [DictEqof K V] -> Bool
(def dicteq-empty? intdict-empty?)

;; dicteq-ref : [DictEqof K V] K -> V
(def (dicteq-ref d k)
  (def a (intdict-ref d (eq?-hash k)))
  (def e (assq k a))
  (cond (e (cdr e))
        (else (error 'dicteq-ref))))

;; dicteq-put : [DictEqof K V] K V -> [DictEqof K V]
(def (dicteq-put d k v)
  (rbtree-update
    d
    (eq?-hash k)
    (lambda (a) (assq-put a k v))
    []))

;; dicteq-update : [DictEqof K V] K [V -> V] V -> [DictEqof K V]
(def (dicteq-update d k f v0)
  (rbtree-update
    d
    (eq?-hash k)
    (lambda (a) (assq-update a k f v0))
    []))

;; dicteq-remove : [DictEqof K V] K -> [DictEqof K V]
(def (dicteq-remove d k)
  (def kh (eq?-hash k))
  (def a (assq-remove (intdict-ref d kh []) k))
  (if (null? a) (intdict-remove d kh) (intdict-put d kh a)))

;; dicteq-has-key? : [DictEqof K V] K -> Bool
(def (dicteq-has-key? d k)
  (assq-has-key? (intdict-ref d (eq?-hash k) []) k))

;; dicteq-keys : [DictEqof K V] -> [Listof K]
(def (dicteq-keys d)
  (for/fold (l []) (a (in-rbtree-values d))
    (append-reverse (assq-keys a) l)))

;; dicteq-put/list : [DictEqof K V] [Listof [Cons K V]] -> [DictEqof K V]
(def (dicteq-put/list d l)
  (cond ((null? l) d)
        (else (dicteq-put/list (dicteq-put d (caar l) (cdar l)) (cdr l)))))

;; list->dicteq : [Listof [Cons K V]] -> [DictEqof K V]
(def (list->dicteq l) (dicteq-put/list empty-dicteq l))

;; dicteq->list : [DictEqof K V] -> [Listof [Cons K V]]
(def (dicteq->list d)
  (for/fold (l []) (vs (in-rbtree-values d))
    (append-reverse vs l)))

;; dicteq=? : [DictEqof Any Any] [DictEqof Any Any] -> Bool
(def (dicteq=? a b (v=? equal?))
  (intdict=? a b
             (lambda (a1 b1)
               (assq=? a1 b1 v=?))))
