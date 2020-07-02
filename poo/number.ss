;;-*- Gerbil -*-
;;; Number Type descriptors for POO and its MOP

;; TODO: a better type hierarchy
;; TODO: clear contracts for + vs add (overflow check vs modular?), etc.
(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact :gerbil/gambit/ports :scheme/base
  :std/iter :std/misc/bytes :std/srfi/1 :std/sugar
  ../utils/base ../utils/io ../utils/number
  ../pure/dict/intdict
  ./poo ./mop ./brace ./io)

;; TODO: basic interface for arithmetics, with proper type signatures.
(.def (Number @ Type.)
  sexp: 'Number
  .element?: number?)

(.def (Integer @ Number)
  sexp: 'Integer
  .element?: exact-integer?
  methods: =>.+ {
    .sexp<-: identity
    .json<-: (lambda (x) (if (<= (integer-length x) 53) x (string<- x)))
    .<-json: (lambda (x) (if (exact-integer? x) x (<-string x)))
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
    .marshal: write-varint
    .unmarshal: read-varint
    <-string: (lambda (x) (validate @ (string->number x)))
    string<-: number->string
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

(.def (Nat @ Integer)
  sexp: 'Nat
  methods: =>.+ {
    .sexp<-: identity
    .marshal: write-varnat
    .unmarshal: read-varnat
    sub: (lambda (x y) (if (>= x y) (- x y) (error "Overflow" - x y)))
    pred: (lambda (x) (if (zero? x) (error "Overflow" pred x) (1- x)))
    non-negative?: true})

(.def (IntegerRange. @ Integer minimum maximum)
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
    .marshal: (λ (n out) (write-integer-bytes n length-in-bytes out))
    .unmarshal: (λ (in) (read-integer-bytes in length-in-bytes))
    normalize: (λ (x) (modulo x n))
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
    length-in-bytes: (n-bytes<-n-bits n-bits)
    normalize: (λ (x) (bitwise-and x maxint))
  })
(def (UInt n-bits) {(:: @ UInt.) (n-bits)})

(.def (JsInt @ Integer)
   sexp: 'JsInt
   .element?: (λ (x) (and (exact-integer? x) (<= .most-negative x .most-positive)))
   .most-positive: (1- (expt 2 53))
   .most-negative: (- .most-positive)
   methods: =>.+ {(:: @@ [un/marshal<-bytes])
    length-in-bytes: 7
    .json<-: identity
    .<-json: (cut validate @ <>)
    .bytes<-: (λ (n) (def bytes (make-bytes 7))
                 (u8vector-sint-set! bytes 0 n big 7)
                 bytes)
    .<-bytes: (λ (bytes) (validate @ (u8vector-sint-ref bytes 0 big 7)))
   })

(def (bytes<-double d)
  (def bytes (make-bytes 8))
  (u8vector-double-set! bytes 0 d big)
  bytes)

(def (double<-bytes bytes)
  (u8vector-double-ref bytes 0 big))

(.def (Real @ Number)
   sexp: 'Real
   .element?: real?
   methods: =>.+ {(:: @@ [un/marshal<-bytes])
    length-in-bytes: 8
    .json<-: identity
    .<-json: (cut validate @ <>)
    .bytes<-: bytes<-double
    .<-bytes: double<-bytes
  })

(.def (IntSet @ Type.)
   sexp: 'IntSet
   Int: Integer
   .element?: (lambda (x) (and (intdict? x)
                          (every (lambda (x)
                                   (and (element? Int (car x))
                                        (eq? #t (cdr x))))
                                 (intdict->list x))))
   methods: =>.+ {(:: methods [bytes<-un/marshal])
     .sexp<-: (lambda (x) `(.call ,sexp .<-list (@list ,@(.list<- x))))
     .json<-: (lambda (x) (map (cut json<- Int <>) (.list<- x)))
     .<-json: (lambda (x) (.<-list (map (cut <-json Int <>) x)))
     .marshal: (lambda (x port)
                 (def l (.list<- x))
                 (marshal Int (length l) port)
                 (for-each (cut marshal Int <> port) l))
     .unmarshal: (lambda (port)
                   (.<-list (for/collect (_ (in-range (unmarshal Int port)))
                              (unmarshal Int port))))
     .empty: empty-intdict
     .empty?: intdict-empty?
     .add: (lambda (x i) (intdict-put x i #t))
     .remove: intdict-remove
     .has?: intdict-has-key?
     .list<-: intdict-keys
     .<-list: (lambda (x) (list->intdict (map (cut cons <> #t) x)))
     .=?: intdict=?
     .min-elt: (compose first-value intdict-min-key)
     .max-elt: (compose first-value intdict-max-key)
   })
