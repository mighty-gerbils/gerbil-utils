;;-*- Gerbil -*-
;;; More types on top of POO and its MOP

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/format :std/iter :std/lazy :std/misc/hash :std/misc/list
  :std/srfi/1 :std/srfi/43 :std/sugar :std/text/hex
  ../utils/base ../utils/hash ../utils/io ../utils/json ../utils/maybe ../utils/number
  ./poo ./mop ./brace ./io ./number)

(.def (Tuple. @ Type. types)
  sexp: `(Tuple ,@(map (cut .@ <> sexp) type-list))
  type-list: (vector->list types)
  .element?:
    (λ (x)
      (def l (vector-length types))
      (and (vector? x) (= (vector-length x) l)
           (let/cc return
             (for ((i (in-iota l)))
               (unless (element? (vector-ref types i) (vector-ref x i)) (return #f)))
             #t)))
  methods: =>.+ {(:: @ [methods.bytes<-marshal])
    .json<-: (lambda (v) (vector->list (vector-map (lambda (_ t x) (json<- t x)) types v)))
    .<-json: (lambda (j) (vector-map (lambda (_ t x) (<-json t x)) types (if (list? j) (list->vector j) j)))
    .marshal: (lambda (v port)
                (vector-for-each (lambda (_ type val) (marshal type val port))
                                 types v))
    .unmarshal: (lambda (port) (vector-map (lambda (_ type) (unmarshal type port)) types))})
(def (Tuple . types) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (cut validate Type <>) types)))
  {(:: @ Tuple.) (types)})

(.def (List. @ Type. type)
  sexp: `(List ,(.@ type sexp))
  .element?: (λ (x) (and (list? x) (every (cut element? type <>) x)))
  methods: {(:: methods [methods.bytes<-marshal methods.string<-json])
    .sexp<-: (lambda (v) ['@list (map (.@ type methods .sexp<-) v) ...])
    .json<-: (let (m (.@ type methods .json<-)) (cut map m <>))
    .<-json: (let (m (.@ type methods .<-json)) (cut map m <>))
    .marshal: (let (m (.@ type methods .marshal))
                (lambda (v port) (write-uint16 (length v) port) (for-each (cut m <> port) v)))
    .unmarshal: (let (m (.@ type methods .unmarshal))
                  (lambda (port) (def l (read-uint16 port))
                     (for/collect (_ (in-range l)) (m port))))})
(def (List type)
  (typecheck Type type)
  {(:: @ List.) (type)})

(.def (Or. @ Type. types)
  sexp: `(Or ,@(map (cut .@ <> name) types))
  .element?: (λ (x) (any (cut element? <> x) types)))
(def (Or . types) {(:: @ Or.) (types)})

(.def (Exactly. @ Type. value)
  sexp: `(Exactly ,(:sexp value)) ;; TODO: have a better generic sexp function?
  .element?: (λ (x) (equal? x value))
  kvalue: (lambda _ value)
  jsvalue: (json-normalize value)
  methods: =>.+ {
    .json<-: (lambda _ jsvalue)
    .<-json: kvalue
    .bytes<-: (lambda _ #u8())
    .<-bytes: kvalue
    .marshal: void
    .unmarshal: kvalue})
(def (Exactly value) {(:: @ Exactly.) (value)})

(define-type Null (Exactly '()))
(define-type False (Exactly #f))
(define-type True (Exactly #t))
(define-type Unit (Exactly (void)))

(.def (Enum. @ Type. vals)
  sexp: `(Enum ,@(map :sexp vals))
  .element?: (cut member <> vals)
  .vals@: (list->vector vals)
  .json@: (list->vector (map json-normalize vals))
  n-bytes: (n-bytes<-n-bits (integer-length (vector-length .vals@)))
  methods: =>.+ {(:: @ [methods.marshal<-bytes])
    length-in-bytes: n-bytes
    .<-nat: (cut vector-ref .vals@ <>)
    .nat<-: (lambda (v) (vector-index (cut equal? <> v) .vals@))
    .json<-: (lambda (v) (vector-ref .json@ (.nat<- v)))
    .<-json: (lambda (j) (vector-index (cut equal? <> j) .json@))
    .bytes<-: (compose (cut bytes<-nat <> length-in-bytes) .nat<-)
    .<-bytes: (compose .<-nat nat<-bytes)})
(defrule (Enum values ...) {(:: @ Enum.) vals: '(values ...)})

(.def (Pair. @ Type. left right)
  sexp: `(Pair ,(.@ left sexp) ,(.@ right sexp))
  methods: =>.+ {(:: @ [methods.bytes<-marshal])
    .json<-: (lambda (v) [(json<- left (car v)) (json<- right (cdr v))])
    .<-json: (lambda (j) (cons (car j) (cadr j)))
    .marshal: (lambda (v port)
                (marshal left (car v) port)
                (marshal right (cdr v) port))
    .unmarshal: (lambda (port) (let* ((a (unmarshal left port))
                                 (d (unmarshal right port)))
                            (cons a d)))})
(def (Pair left right) {(:: @ Pair.) (left) (right)})

(.def (Maybe. @ Type. type)
  sexp: `(Maybe ,(.@ type sexp))
  .element?: (lambda (x) (or (eq? x null) (element? type x)))
  methods: {(:: @@ [methods.bytes<-marshal])
    .sexp<-: (lambda (v) (if (eq? v null) 'null (sexp<- type v)))
    .json<-: (lambda (v) (if (eq? v null) v ((.@ type methods .json<-) v)))
    .<-json: (lambda (j) (if (eq? j null) j ((.@ type methods .<-json) j)))
    .marshal: (λ (x port) (cond ((eq? x null) (write-byte 0 port))
                                (else (write-byte 1 port) (marshal type x port))))
    .unmarshal: (λ (port) (if (zero? (read-byte port)) null (unmarshal type port)))})
(def (Maybe type) {(:: @ Maybe.) (type)})

(.def (Map. @ Type. Key Value)
  sexp: `(Map ,(.@ Value sexp) <- ,(.@ Key sexp))
  .element?: (lambda (x) (and (hash-table? x)
                         (let/cc return
                           (hash-for-each (lambda (k v) (unless (and (element? Key k)
                                                                (element? Value v))
                                                     (return #f)))
                                          x) #t)))
  methods: =>.+ {(:: @@ methods.string&bytes&marshal<-json)
    .empty: (make-hash-table)
    .json<-: (lambda (m) (hash-key-value-map m (cut string<- Key <>) (cut json<- Value <>)))
    .<-json: (lambda (j) (hash-key-value-map j (cut <-string Key <>) (cut <-json Value <>)))})
(defrules Map (<- ->)
  ((_ Value <- Key) {(:: @ Map.) Key: Key Value: Value})
  ((_ Key -> Value) {(:: @ Map.) Key: Key Value: Value}))

(.def methods.bytes
  .sexp<-: (lambda (x) ['hex-decode (hex-encode x)])
  .string<-: hex-encode
  .<-string: hex-decode
  .bytes<-: identity
  .<-bytes: identity
  .json<-: .string<-
  .<-json: .<-string)
(.def (Bytes @ Type.)
  sexp: 'Bytes
  .element?: bytes?
  methods: =>.+ {(:: @@ [methods.bytes])
    .Length: Nat
    .zero: #u8()
    .marshal: (lambda (x port) (marshal .Length (bytes-length x) port) (write-u8vector x port))
    .unmarshal: (lambda (port) (def n (unmarshal .Length port)) (read-bytes n port))})
(.def (BytesN. @ Type. n)
  sexp: `(BytesN ,n)
  .element?: (λ (x) (and (bytes? x) (= (bytes-length x) n)))
  methods: =>.+ {(:: @@ [methods.bytes])
    length-in-bytes: n
    .zero: (make-bytes n)
    .<-string: (λ (x) (validate @ (hex-decode x)))
    .<-bytes: (cut validate @ <>)
    .marshal: write-u8vector
    .unmarshal: (cut read-bytes n <>)})
(def (BytesN n) (.cc BytesN. n: n))

(.def (String @ Type.) sexp: 'String .element?: string?
  methods: {(:: @ methods.marshal<-bytes)
    .Bytes: Bytes
    .zero: ""
    .sexp<-: identity
    .<-string: identity
    .string<-: identity
    .bytes<-: string->bytes
    .<-bytes: bytes->string
    .<-json: (cut validate @ <>)
    .json<-: identity})
(.def (methods.bytes&marshal<-string @ [methods.bytes<-marshal] .<-string .string<-)
  String: String
  .marshal: (lambda (x port) (marshal String (.string<- x) port))
  .unmarshal: (lambda (port) (.<-string (unmarshal String port))))
(.def (methods.json&bytes&marshal<-string @ [methods.bytes&marshal<-string] .<-string .string<-)
  .<-json: .<-string
  .json<-: .string<-)
(.def (Symbol @ Type.) sexp: 'Symbol .element?: symbol?
  methods: =>.+ {(:: @@ methods.json&bytes&marshal<-string)
    .<-string: string->symbol
    .string<-: symbol->string})
(.def (Keyword @ Type.) sexp: 'Keyword .element?: keyword?
  methods: =>.+ {(:: @@ methods.json&bytes&marshal<-string)
    .<-string: string->keyword
    .string<-: keyword->string})
(.def (Json @ Type.)
   sexp: 'Json
   .element?: true
   methods: =>.+ {(:: @@ methods.bytes&marshal<-string)
    .json<-: identity
    .<-json: identity
    .string<-: string<-json
    .<-string: json<-string})
(.def (methods.string&bytes&marshal<-json @ methods.bytes&marshal<-string .json<- .<-json)
   .string<-: (compose string<-json .json<-)
   .<-string: (compose .<-json json<-string))
