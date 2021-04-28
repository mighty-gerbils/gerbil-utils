(export empty-rationaldict
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
        rationaldict-min-key
        rationaldict-max-key)

(import :std/iter
        :std/misc/rbtree)

;; Functional Dictionaries mapping rational-number keys to values

;; An [Rationaldictof V] is an [Rbtreeof Rational V]

;; empty-rationaldict : [Rationaldictof V]
(def empty-rationaldict (make-rbtree -))

;; rationaldict-empty? : [Rationaldictof V] -> Bool
(def rationaldict-empty? rbtree-empty?)

;; private unexported value for notfound
(def notfound (gensym 'notfound))

;; rationaldict-ref : [Rationaldictof V] Rational ?[-> V] -> V
(def (rationaldict-ref r k (default (cut error "rationaldict-ref: No value associated with key" r k)))
  (def v (rbtree-ref r k notfound))
  (if (eq? v notfound) (default) v))

;; rationaldict-get : [Rationaldictof V] Rational ?V -> V
(def (rationaldict-get r k (default #f))
  (rbtree-ref r k default))

;; rationaldict-put : [Rationaldictof V] Rational V -> [Rationaldictof V]
(def rationaldict-put rbtree-put)

;; rationaldict-update : [Rationaldictof V] Rational [V -> V] V -> [Rationaldictof V]
(def rationaldict-update rbtree-update)

;; rationaldict-remove : [Rationaldictof V] Rational -> [Rationaldictof V]
(def rationaldict-remove rbtree-remove)

;; rationaldict-has-key? : [Rationaldictof V] Rational -> Bool
(def (rationaldict-has-key? d k)
  (unless (rbtree? d) (error "rationaldict-has-key?: expected a rationaldict as 1st argument"))
  (unless (rational? k) (error "rationaldict-has-key?: expected a rational as 2nd argument"))
  (not (eq? notfound (rbtree-ref d k notfound))))

;; rationaldict-keys : [Rationaldictof V] -> [Listof Rational]
(def (rationaldict-keys d) (for/collect (k (in-rbtree-keys d)) k))

;; rationaldict-values : [Rationaldictof V] -> [Listof V]
(def (rationaldict-values d) (for/collect (v (in-rbtree-values d)) v))

;; rationaldict-put/list : [Rationaldictof V] [Listof [Cons Rational V]] -> [Rationaldictof V]
(def (rationaldict-put/list d l)
  (cond ((null? l) d)
        (else (rationaldict-put/list (rationaldict-put d (caar l) (cdar l)) (cdr l)))))

;; list->rationaldict : [Listof [Cons Rational V]] -> [Rationaldictof V]
(def (list->rationaldict l) (rationaldict-put/list empty-rationaldict l))

;; rationaldict->list : [Rationaldictof V] -> [Listof [Cons Rational V]]
(def rationaldict->list rbtree->list)

;; rationaldict=? : [Rationaldictof Any] [Rationaldictof Any] -> Bool
(def (rationaldict=? a b (v=? equal?))
  (def aks (rationaldict-keys a))
  (def bks (rationaldict-keys b))
  (and (= (length aks) (length bks))
       (andmap (lambda (k)
                 (and (rationaldict-has-key? b k)
                      (v=? (rationaldict-ref a k) (rationaldict-ref b k))))
               aks)))

;; rationaldict-min-key : [Rationaldictof V] ?X -> [Or Rational X]
(def (rationaldict-min-key a (default #f))
  (let/cc return
    (rbtree-for-each return a)
    default))

;; rationaldict-max-key : [Rationaldictof V] ?X -> [Or Rational X]
(def (rationaldict-max-key a (default #f))
  (let/cc return
    (rbtree-for-eachr return a)
    default))
