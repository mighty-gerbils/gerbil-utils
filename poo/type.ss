;;-*- Gerbil -*-
;;; More types on top of POO and its MOP

(export #t)
(import :clan/utils/debug)

(import
  :gerbil/gambit/bits :gerbil/gambit/exact :gerbil/gambit/ports :scheme/base
  :std/format :std/generic :std/iter :std/lazy :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/hash :clan/utils/io :clan/utils/number
  :clan/poo/poo :clan/poo/mop :clan/poo/brace :clan/poo/io)

(.def (Tuple. @ Type. types)
  sexp: `(Tuple ,@(map (cut .@ <> sexp) (vector->list types)))
  .element?:
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
  sexp: `(IntegerRange ,@(if minimum `(min: ,minimum) '())
                       ,@(if max `(max: ,maximum) '()))
  .element?:
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
  sexp: `(List ,(.@ type name))
  .element?: (λ (x) (and (list? x) (every (cut element? type <>) x))))
(def (List type)
  (typecheck Type type)
  {(:: @ List.) (type)})

(.def (Or. @ Type. types)
  sexp: `(Or ,@(map (cut .@ <> name) types))
  .element?: (λ (x) (any (cut element? <> x) types)))
(def (Or . types) {(:: @ Or.) (types)})

(.def (Exactly. @ Type. value)
  sexp: `(Exactly ,(:sexp value)) ;; TODO: have a better generic sexp function?
  .element?: (λ (x) (equal? x value)))
(def (Exactly value) {(:: @ Exactly.) (value)})

(def Null (Exactly '()))
(def False (Exactly #f))
(def True (Exactly #t))

(.def (OneOf. @ Type. values)
  sexp: `(OneOf ,@(map :sexp values))
  .element?: (λ (x) (member x values)))

(def (OneOf . values) {(:: @ OneOf.) (values)})

(.def (Pair. @ Type. left right)
  sexp: `(Pair ,(.@ left sexp) ,(.@ right sexp)))
(def (Pair left right) {(:: @ Pair.) (left) (right)})

(.def (Integer @ Type.)
  sexp: 'Z
  .element?: exact-integer?
  methods: =>.+ {
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
    lognot: bitwise-not ;; more bitwise operations, see Gambit
    shift-left: arithmetic-shift
    shift-right: (λ (x n) (arithmetic-shift x (- n)))
    marshal: write-varint
    unmarshal: read-varint
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
    non-negative?: (cut <= 0 <>)
    =?: (cut = <> <>)
    sign: (cut number-comparer <> 0)
    ;; extract-bit-field test-bit-field? integer-length integer-length<? ...
    max: max
    min: min})

#;(def ZZ
  {(:: @ [] n)
  maxint: (- n 1)
  length-in-bits: (integer-length maxint)})

(def (unary-pre-op-check op check info x)
  (if (check x) (op x)
      (error "attempted operation but the arguments are out of range"
        (car info) (cdr info) x)))

(def (binary-pre-op-check op check info x y)
  (if (check x y) (op x y)
      (error "attempted operation but the arguments are out of range"
        (car info) (cdr info) x y)))

(def (unary-post-op-check op check info x)
  (def y (op x))
  (if (check y) y
      (error "attempted operation but the arguments are out of range"
        (car info) (cdr info) x)))

(def (binary-post-op-check op check info x y)
  (def z (op x y))
  (if (check z) z
      (error "attempted operation but the arguments are out of range"
        (car info) (cdr info) x y)))

(.def (Z/. @ Integer n)
  sexp: `(Z/ ,n)
  .element?: (nat-under? n)
  methods: =>.+ {
    normalize: (λ (x) (modulo x n)) ;; TODO: figure why using a keyword fails
    maxint: (- n 1)
    length-in-bits: (integer-length maxint)
    length-in-bytes: (integer-length-in-bytes maxint)
    add-carry?: (λ (x y) (<= n (+ x y)))
    add-negative-overflow?: false
    sub-carry?: false
    sub-negative-overflow?: (λ (x y) (< x y))
    add: (λ (x y) (def z (+ x y)) (if (<= z maxint) z (- z n)))
    sub: (λ (x y) (def z (- x y)) (if (<= 0 z) z (+ z n)))
    mul: (λ (x y) (normalize (* x y)))
    write-to-bytes: (λ (out n) (write-integer-bytes out n length-in-bytes))
    read-from-bytes: (λ (in) (read-integer-bytes in length-in-bytes))
    <-string: string->number
    ->string: number->string
    succ: (λ (x) (if (= x maxint) 0 (1+ x)))
    pred: (λ (x) (if (zero? x) maxint (1- x)))
    ;; function taking two entries a, b.
    ;; -- If a = b then returns 0;
    ;; -- If a > b then returns 1
    ;; -- If a < b then returns -1
    ;; (-- If the numbers are not comparable, returns #f)
    comparer: number-comparer
    ;; TODO: range-checked arithmetics
    ;; add-valid? sum? mul-valid? product?
    max: max
    min: min})

(def (Z/ n) {(:: @ Z/.) (n)})

(.def (UInt. @ Z/. n-bits)
  n: (arithmetic-shift 1 n-bits)
  sexp: `(UInt ,n-bits)
  .element?: (lambda (x) (and (nat? x) (<= (integer-length x) n-bits)))
  methods: =>.+ {(:: @ [] maxint)
    length-in-bits: n-bits
    length-in-bytes: (arithmetic-shift (+ n-bits 7) -3)
    normalize: (λ (x) (bitwise-and x maxint))
  })

(def (UInt n-bits) {(:: @ UInt.) (n-bits)})

(.def (Symbol @ Type.) sexp: 'Symbol .element?: symbol?)
(.def (String @ Type.) sexp: 'String .element?: string?)
(.def (Number @ Type.) sexp: 'Number .element?: number?)
