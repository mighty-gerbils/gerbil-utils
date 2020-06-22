(export empty-intdict
        intdict-empty?
        intdict-ref
        intdict-put
        intdict-update
        intdict-remove
        intdict-has-key?
        intdict-keys
        intdict-put/list
        list->intdict
        intdict->list
        intdict=?
        intdict-min-key
        intdict-max-key)

(import :std/iter
        :std/misc/rbtree)

;; Functional Dictionaries mapping integer keys to values

;; An [Intdictof V] is an [Rbtreeof Int V]

;; empty-intdict : [Intdictof V]
(def empty-intdict (make-rbtree -))

;; intdict-empty? : [Intdictof V] -> Bool
(def intdict-empty? rbtree-empty?)

;; intdict-ref : [Intdictof V] Int -> V
(def intdict-ref rbtree-ref)

;; intdict-put : [Intdictof V] Int V -> [Intdictof V]
(def intdict-put rbtree-put)

;; intdict-update : [Intdictof V] Int [V -> V] V -> [Intdictof V]
(def intdict-update rbtree-update)

;; intdict-remove : [Intdictof V] Int -> [Intdictof V]
(def intdict-remove rbtree-remove)

;; intdict-has-key? : [Intdictof V] Int -> Bool
(def (intdict-has-key? d k)
  (def notfound (gensym 'notfound))
  (not (eq? notfound (rbtree-ref d k notfound))))

;; intdict-keys : [Intdictof V] -> [Listof Int]
(def (intdict-keys d) (for/collect (k (in-rbtree-keys d)) k))

;; intdict-put/list : [Intdictof V] [Listof [Cons Int V]] -> [Intdictof V]
(def (intdict-put/list d l)
  (cond ((null? l) d)
        (else (intdict-put/list (intdict-put d (caar l) (cdar l)) (cdr l)))))

;; list->intdict : [Listof [Cons Int V]] -> [Intdictof V]
(def (list->intdict l) (intdict-put/list empty-intdict l))

;; intdict->list : [Intdictof V] -> [Listof [Cons Int V]]
(def intdict->list rbtree->list)

;; intdict=? : [Intdictof Any] [Intdictof Any] -> Bool
(def (intdict=? a b (v=? equal?))
  (def aks (intdict-keys a))
  (def bks (intdict-keys b))
  (and (= (length aks) (length bks))
       (andmap (lambda (k)
                 (and (intdict-has-key? b k)
                      (v=? (intdict-ref a k) (intdict-ref b k))))
               aks)))

;; intdict-min-key : [Intdictof V] ?X -> [Or Int X]
(def (intdict-min-key a (default #f))
  (let/cc return
    (rbtree-for-each return a)
    default))

;; intdict-max-key : [Intdictof V] ?X -> [Or Int X]
(def (intdict-max-key a (default #f))
  (let/cc return
    (rbtree-for-eachr return a)
    default))
