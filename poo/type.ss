;;-*- Gerbil -*-
;;; More types on top of POO and its MOP

(export #t)
(import :clan/utils/debug)

(import
  :gerbil/gambit/exact :gerbil/gambit/ports
  :std/format :std/iter :std/lazy :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/hash :clan/poo/poo :clan/poo/mop)

(.def (Tuple. @ Type. types)
  (name (symbolify `(Tuple ,@(map (cut .@ <> name) (vector->list types)))))
  (element?
    (λ (x)
      (def l (vector-length types))
      (and (vector? x) (= (vector-length x) l)
           (let/cc return
             (for ((i (in-iota l)))
               (unless (element? (vector-ref types i) (vector-ref x i)) (return #f)))
             #t)))))

(def (Tuple . types) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (cut validate Type <>) types)))
  (.o (:: @ Tuple.) (types)))

(.def (IntegerRange. @ Type. minimum maximum)
  (name (symbolify `(IntegerRange ,@(if minimum `(min: ,minimum) '())
                                  ,@(if max `(max: ,maximum) '()))))
  (element?
   (match (vector minimum maximum)
     ((vector #f #f) exact-integer?)
     ((vector _ #f) (λ (x) (and (exact-integer? x) (<= minimum x))))
     ((vector #f _) (λ (x) (and (exact-integer? x) (<= x maximum))))
     ((vector _ _) (λ (x) (and (exact-integer? x) (<= minimum x maximum)))))))

(def (IntegerRange min: (minimum #f) max: (maximum #f))
  (assert! (or (not minimum) (exact-integer? minimum)))
  (assert! (or (not maximum) (exact-integer? maximum)))
  (.o (:: @ IntegerRange.) (minimum) (maximum)))

(.def (List. @ Type. type)
  (name (symbolify `(List ,(.@ type name))))
  (element? (λ (x) (and (list? x) (every (cut element? type <>) x)))))

(def (List type)
  (typecheck Type type)
  (.o (:: @ List.) (type)))

(.def (Or. @ Type. types)
  (name (symbolify `(Or ,@(map (cut .@ <> name) types))))
  (element? (λ (x) (any (cut element? <> x) types))))

(.def (Exactly. @ Type. value)
  (name (symbolify `(Exactly ,(repr value))))
  (element? (λ (x) (equal? x value))))

(def (Exactly value) (.o (:: @ Exactly.) (value)))

(def Null (Exactly '()))
(def False (Exactly #f))
(def True (Exactly #t))

(.def (OneOf. @ Type. values)
  (name (symbolify `(Or ,@(map repr values))))
  (element? (λ (x) (member x values))))

(def (OneOf . values) (.o (:: @ OneOf.) (values)))

(.def (Pair. @ Type. left right)
  (name (symbolify `(Pair ,(.@ left name) ,(.@ right name)))))
(def (Pair left right) (.o (:: @ Pair.) (left) (right)))

