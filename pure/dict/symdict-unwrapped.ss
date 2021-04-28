(export empty-symdict
        symdict-empty?
        symdict-ref
        symdict-get
        symdict-put
        symdict-update
        symdict-remove
        symdict-has-key?
        symdict-keys
        symdict-values
        symdict-put/list
        list->symdict
        symdict->list
        symdict=?)

(import :std/iter
        :std/misc/rbtree)

;; Functional Dictionaries mapping Symbol keys to values

;; A [Symdictof V] is an [Rbtreeof Symbol V]

;; empty-symdict : [Symdictof V]
(def empty-symdict (make-rbtree symbol-hash-cmp))

;; symdict-empty? : [Symdictof V] -> Bool
(def symdict-empty? rbtree-empty?)

;; private unexported value for notfound
(def notfound (gensym 'notfound))

;; symdict-ref : [Symdictof V] Symbol ?[-> V] -> V
(def (symdict-ref r k (default (cut error "symdict-ref: No value associated with key" r k)))
  (def v (rbtree-ref r k notfound))
  (if (eq? v notfound) (default) v))

;; symdict-get : [Symdictof V] Symbol ?V -> V
(def (symdict-get r k (default #f))
  (rbtree-ref r k default))

;; symdict-put : [Symdictof V] Symbol V -> [Symdictof V]
(def symdict-put rbtree-put)

;; symdict-update : [Symdictof V] Symbol [V -> V] V -> [Symdictof V]
(def symdict-update rbtree-update)

;; symdict-remove : [Symdictof V] Symbol -> [Symdictof V]
(def symdict-remove rbtree-remove)

;; symdict-has-key? : [Symdictof V] Symbol -> Bool
(def (symdict-has-key? d k)
  (unless (rbtree? d) (error "symdict-has-key?: expected a symdict as 1st argument"))
  (unless (symbol? k) (error "symdict-has-key?: expected a symbol as 2nd argument"))
  (not (eq? notfound (rbtree-ref d k notfound))))

;; symdict-keys : [Symdictof V] -> [Listof Symbol]
(def (symdict-keys d) (for/collect (k (in-rbtree-keys d)) k))

;; symdict-values : [Symdictof V] -> [Listof V]
(def (symdict-values d) (for/collect (k (in-rbtree-values d)) k))

;; symdict-put/list : [Symdictof V] [Listof [Cons Symbol V]] -> [Symdictof V]
(def (symdict-put/list d l)
  (cond ((null? l) d)
        (else (symdict-put/list (symdict-put d (caar l) (cdar l)) (cdr l)))))

;; list->symdict : [Listof [Cons Symbol V]] -> [Symdictof V]
(def (list->symdict l) (symdict-put/list empty-symdict l))

;; symdict->list : [Symdictof V] -> [Listof [Cons Symbol V]]
(def symdict->list rbtree->list)

;; symdict=? : [Symdictof Any] [Symdictof Any] -> Bool
(def (symdict=? a b (v=? equal?))
  (def aks (symdict-keys a))
  (def bks (symdict-keys b))
  (and (= (length aks) (length bks))
       (andmap (lambda (k)
                 (and (symdict-has-key? b k)
                      (v=? (symdict-ref a k) (symdict-ref b k))))
               aks)))
