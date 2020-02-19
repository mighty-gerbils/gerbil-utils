;;-*- Gerbil -*-
;;; More types on top of POO and its MOP

(export #t)
(import :clan/utils/debug)

(import
  :gerbil/gambit/bits :gerbil/gambit/exact :gerbil/gambit/ports :scheme/base
  :std/format :std/iter :std/lazy :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/hash :clan/utils/number
  :clan/poo/poo :clan/poo/mop :clan/poo/brace :clan/poo/io)

(.def (Tuple. @ Type. types)
  name: (symbolify `(Tuple ,@(map (cut .@ <> name) (vector->list types))))
  element?:
    (λ (x)
      (def l (vector-length types))
      (and (vector? x) (= (vector-length x) l)
           (let/cc return
             (for ((i (in-iota l)))
               (unless (element? (vector-ref types i) (vector-ref x i)) (return #f)))
             #t))))
(def (Tuple . types) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (cut validate Type <>) types)))
  {(:: @ Tuple.) (types)})

(.def (IntegerRange. @ Type. minimum maximum)
  name: (symbolify `(IntegerRange ,@(if minimum `(min: ,minimum) '())
                                  ,@(if max `(max: ,maximum) '())))
  element?:
   (match (vector minimum maximum)
     ((vector #f #f) exact-integer?)
     ((vector _ #f) (λ (x) (and (exact-integer? x) (<= minimum x))))
     ((vector #f _) (λ (x) (and (exact-integer? x) (<= x maximum))))
     ((vector _ _) (λ (x) (and (exact-integer? x) (<= minimum x maximum))))))
(def (IntegerRange min: (minimum #f) max: (maximum #f))
  (assert! (or (not minimum) (exact-integer? minimum)))
  (assert! (or (not maximum) (exact-integer? maximum)))
  {(:: @ IntegerRange.) (minimum) (maximum)})

(.def (List. @ Type. type)
  name: (symbolify `(List ,(.@ type name)))
  element?: (λ (x) (and (list? x) (every (cut element? type <>) x))))
(def (List type)
  (typecheck Type type)
  {(:: @ List.) (type)})

(.def (Or. @ Type. types)
  name: (symbolify `(Or ,@(map (cut .@ <> name) types)))
  element?: (λ (x) (any (cut element? <> x) types)))
(def (Or . types) {(:: @ Or.) (types)})

(.def (Exactly. @ Type. value)
  name: (symbolify `(Exactly ,(repr value)))
  element?: (λ (x) (equal? x value)))
(def (Exactly value) {(:: @ Exactly.) (value)})

(def Null (Exactly '()))
(def False (Exactly #f))
(def True (Exactly #t))

(.def (OneOf. @ Type. values)
  name: (symbolify `(Or ,@(map repr values)))
  element?: (λ (x) (member x values)))

(def (OneOf . values) {(:: @ OneOf.) (values)})

(.def (Pair. @ Type. left right)
  name: (symbolify `(Pair ,(.@ left name) ,(.@ right name))))
(def (Pair left right) {(:: @ Pair.) (left) (right)})

(.def (Z. @ Type.)
  name: 'Z
  element?: exact-integer?
  add: +
  sub: -
  mul: *
  div: floor-quotient
  mod: modulo
  zero: 0
  one: 1
  logand: bitwise-and
  logor: bitwise-ior
  logxor: bitwise-xor
  lognot: bitwise-not
  shift-left: arithmetic-shift
  shift-right: (λ (x n) (arithmetic-shift x (- n)))
  write-to-bytes: write-varint
  read-from-bytes: read-varint
  <-string: string->number
  ->string: number->string
  succ: 1+
  pred: 1-
  ;; function taking two entries a, b.
  ;; -- If a = b then returns 0;
  ;; -- If a > b then returns 1
  ;; -- If a < b then returns -1
  ;; (-- If the numbers are not comparable, returns #f)
  comparer: number-comparer
  max: max
  min: min)


'(def ZZ
  (.o (:: @ [] n)
  maxint: (- n 1)
  length-in-bits: (integer-length maxint)))

(.def (Z/. @ Z. n)
  name (symbolify `(Z/ ,(.@ n)))
  element? (nat-under? n)
  normalize (λ (x) (modulo x n)) ;; TODO: figure why using a keyword fails
  maxint (- n 1)
  length-in-bits (integer-length maxint)
  length-in-bytes (integer-length-in-bytes maxint)

  add-carry?: (λ (x y) (<= n (+ x y)))
  add-negative-overflow?: (λ (x y) #f)
  sub-carry?: false
  sub-negative-overflow?: (λ (x y) (< x y))
  add: (λ (x y) (normalize (+ x y)))
  sub: (λ (x y) (normalize (- x y)))
  mul: (λ (x y) (normalize (* x y)))
  write-to-bytes: (λ (out n) (write-integer-bytes out n length-in-bytes))
  read-from-bytes: (λ (in) (read-integer-bytes in length-in-bytes))
  <-string: string->number
  ->string: number->string
  succ: 1+
  pred: 1-
  ;; function taking two entries a, b.
  ;; -- If a = b then returns 0;
  ;; -- If a > b then returns 1
  ;; -- If a < b then returns -1
  ;; (-- If the numbers are not comparable, returns #f)
  comparer: number-comparer
  max: max
  min: min)
