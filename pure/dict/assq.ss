(export assq-ref
        assq-get
        assq-put
        assq-update
        assq-remove
        assq-has-key?
        assq-keys
        assq-values
        assq-put/list
        list->assq
        assq=?
        assq->repr-sexpr
        repr-sexpr->assq)

(import :std/iter)

;; An [Assqof K V] is a [Listof [Cons K V]]
;; where the keys should be compared by `eq?`,
;; and there are no duplicate keys

;; assq-ref : [Assqof K V] K ?[-> V] -> V
(def (assq-ref a k (default (cut error "assq-ref: No value associated with key" a k)))
  (def e (assq k a))
  (if e (cdr e) (default)))

;; assq-get : [Assqof K V] K ?V -> V
(def (assq-get a k (default #f))
  (def e (assq k a))
  (if e (cdr e) default))

;; assq-put : [Assqof K V] K V -> [Assqof K V]
;; do not introduce duplicate keys, if key exists remove old entry
(def (assq-put a k v)
  (def e (assq k a))
  (cons (cons k v) (if e (remq e a) a)))

;; assq-update : [Assqof K V] K [V -> V] V -> [Assqof K V]
;; do not introduce duplicate keys, if key exists remove old entry
(def (assq-update a k f v0)
  (def e (assq k a))
  (cond (e    (cons (cons k (f (cdr e))) (remq e a)))
        (else (cons (cons k (f v0)) a))))

;; assq-remove : [Assqof K V] K -> [Assqof K V]
(def (assq-remove a k)
  (def e (assq k a))
  (if e (remq e a) a))

;; assq-has-key? : [Assqof K V] K -> Bool
(def (assq-has-key? a k)
  (and (assq k a) #t))

;; assq-keys : [Assqof K V] -> [Listof K]
(def (assq-keys a) (map car a))

;; assq-values : [Assqof K V] -> [Listof V]
(def (assq-values a) (map cdr a))

;; assq-put/list : [Assqof K V] [Listof [Cons K V]] -> [Assqof K V]
;; `a` has no duplicate keys, but `l` can
(def (assq-put/list a l)
  (cond ((null? l) a)
        (else (assq-put/list (assq-put a (caar l) (cdar l)) (cdr l)))))

;; list->assq : [Listof [Cons K V]] -> [Assqof K V]
;; output has no duplicate keys
(def (list->assq l) (assq-put/list [] l))

;; assq=? : [Assqof Any Any] [Assqof Any Any] -> Bool
;; can assume each has no duplicate keys, but possibly different order
(def (assq=? a b (v=? equal?))
  (and (= (length a) (length b))
       (andmap (lambda (ae)
                 (def be (assq (car ae) b))
                 (and be (v=? (cdr ae) (cdr be))))
               a)))

;; assq->repr-sexpr : [K -> Sexpr] [V -> Sexpr] -> [[Assqof K V] -> Sexpr]
(def ((assq->repr-sexpr k->s v->s) a)
  (cons '@list
        (for/collect ((e a))
          ['cons (k->s (car e)) (v->s (cdr e))])))

;; repr-sexpr->assq : [Sexpr -> K] [Sexpr -> V] -> [Sexpr -> [Assqof K V]]
(def ((repr-sexpr->assq s->k s->v) s)
  (match s
    ((cons '@list l)
     (for/collect (e l)
       (match e
         (['cons ks vs]
          (cons (s->k ks) (s->v vs)))
         (_ (error "repr-sexpr->assq: bad entry shape, expected `cons`" e)))))
    (_ (error "repr-sexpr->assq: expected `@list`" s))))
