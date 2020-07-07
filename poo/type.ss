;;-*- Gerbil -*-
;;; More types on top of POO and its MOP

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/format :std/iter :std/lazy :std/misc/hash :std/misc/list
  :std/srfi/1 :std/srfi/43 :std/sugar :std/text/hex
  ../utils/base ../utils/hash ../utils/io ../utils/json ../utils/maybe ../utils/number
  ./poo ./mop ./brace ./io ./number)

(.def (Tuple. @ [methods.bytes<-marshal Type.] types)
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
  .json<-: (lambda (v) (vector->list (vector-map (lambda (_ t x) (json<- t x)) types v)))
  .<-json: (lambda (j) (vector-map (lambda (_ t x) (<-json t x)) types (if (list? j) (list->vector j) j)))
  .marshal: (lambda (v port)
              (vector-for-each (lambda (_ type val) (marshal type val port))
                               types v))
  .unmarshal: (lambda (port) (vector-map (lambda (_ type) (unmarshal type port)) types)))
(def (Tuple . types) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (cut validate Type <>) types)))
  {(:: @ Tuple.) (types)})

(.def (List. @ [methods.bytes<-marshal methods.string<-json Type.] type)
  sexp: `(List ,(.@ type sexp))
  .element?: (λ (x) (and (list? x) (every (cut element? type <>) x)))
  .sexp<-: (lambda (v) ['@list (map (.@ type .sexp<-) v) ...])
  .json<-: (let (m (.@ type .json<-)) (cut map m <>))
  .<-json: (let (m (.@ type .<-json)) (cut map m <>))
  .marshal: (let (m (.@ type .marshal))
              (lambda (v port) (write-uint16 (length v) port) (for-each (cut m <> port) v)))
  .unmarshal: (let (m (.@ type .unmarshal))
                (lambda (port) (def l (read-uint16 port))
                   (for/collect (_ (in-range l)) (m port)))))
(def (List type)
  (typecheck Type type)
  {(:: @ List.) (type)})

(.def methods.bytes
  .sexp<-: (lambda (x) ['hex-decode (hex-encode x)])
  .string<-: hex-encode
  .<-string: hex-decode
  .bytes<-: identity
  .<-bytes: identity
  .json<-: .string<-
  .<-json: .<-string)
(.def (Bytes @ [methods.bytes Type.])
  sexp: 'Bytes
  .element?: bytes?
  .Length: Nat
  .zero: #u8()
  .marshal: (lambda (x port) (marshal .Length (bytes-length x) port) (write-u8vector x port))
  .unmarshal: (lambda (port) (def n (unmarshal .Length port)) (read-bytes* n port)))
(.def (BytesN. @ [methods.bytes Type.] n)
  sexp: `(BytesN ,n)
  .element?: (λ (x) (and (bytes? x) (= (bytes-length x) n)))
  .length-in-bytes: n
  .zero: (make-bytes n)
  .<-string: (λ (x) (validate @ (hex-decode x)))
  .<-bytes: (cut validate @ <>)
  .marshal: write-u8vector
  .unmarshal: (cut read-bytes* n <>))
(def (BytesN n) (.cc BytesN. n: n))

(.def (String @ [methods.marshal<-bytes Type.])
  sexp: 'String
  .element?: string?
  .Bytes: Bytes
  .zero: ""
  .sexp<-: identity
  .<-string: identity
  .string<-: identity
  .bytes<-: string->bytes
  .<-bytes: bytes->string
  .<-json: (cut validate @ <>)
  .json<-: identity)
(.def (methods.bytes&marshal<-string @ [methods.bytes<-marshal] .<-string .string<-)
  .String: String
  .marshal: (lambda (x port) (marshal .String (.string<- x) port))
  .unmarshal: (lambda (port) (.<-string (unmarshal .String port))))
(.def (methods.json&bytes&marshal<-string @ [methods.bytes&marshal<-string] .<-string .string<-)
  .<-json: .<-string
  .json<-: .string<-)
(.def (Symbol @ [methods.json&bytes&marshal<-string Type.])
  sexp: 'Symbol
  .element?: symbol?
  .<-string: string->symbol
  .string<-: symbol->string)
(.def (Keyword @ [methods.json&bytes&marshal<-string Type.])
  exp: 'Keyword
  .element?: keyword?
  .<-string: string->keyword
  .string<-: keyword->string)
(.def (Json @ [methods.bytes&marshal<-string Type.])
  sexp: 'Json
  .element?: true
  .sexp<-: identity ;; TODO: recursively handle tables
  .json<-: identity
  .<-json: identity
  .string<-: string<-json
  .<-string: json<-string)
(.def (methods.string&bytes&marshal<-json @ [methods.bytes&marshal<-string] .json<- .<-json)
  .string<-: (compose string<-json .json<-)
  .<-string: (compose .<-json json<-string))

(.def (Or. @ [methods.bytes<-marshal Type.] types)
  sexp: `(Or ,@(map (cut .@ <> sexp) types))
  types@: (list->vector types)
  .element?: (λ (x) (any (cut element? <> x) types))
  .discriminant-length-in-bits: (integer-length (1- (length types)))
  .discriminant-length-in-bytes: (n-bytes<-n-bits .discriminant-length-in-bits)
  .discriminant<-: (lambda (v) (let/cc return
                            (vector-for-each (lambda (i t) (when (element? t v) (return i))) types@) #f))
  .json<-: (lambda (v) (def disc (.discriminant<- v))
              [disc (json<- (vector-ref types@ disc) v)])
  .<-json: (lambda (j) (<-json (vector-ref types@ (car j)) (cadr j))) ;; TODO: validate the json?
  .marshal: (lambda (v port) (def disc (.discriminant<- v))
              (write-integer-bytes disc .discriminant-length-in-bytes port)
              (marshal (vector-ref types@ disc) v port))
  .unmarshal: (lambda (port) (def disc (read-integer-bytes .discriminant-length-in-bytes port))
                (unmarshal (vector-ref types@ disc) port)))
(def (Or . types) {(:: @ Or.) (types)})

(.def (Exactly. @ Type. value)
  sexp: `(Exactly ,(:sexp value)) ;; TODO: have a better generic sexp function?
  .element?: (λ (x) (equal? x value))
  kvalue: (lambda _ value)
  jsvalue: (json-normalize value)
  .json<-: (lambda _ jsvalue)
  .<-json: kvalue
  .bytes<-: (lambda _ #u8())
  .<-bytes: kvalue
  .marshal: void
  .unmarshal: kvalue)
(def (Exactly value) {(:: @ Exactly.) (value)})

(define-type Null (Exactly '()))
(define-type False (Exactly #f))
(define-type True (Exactly #t))
(define-type Unit (Exactly (void)))

(.def (Enum. @ [methods.marshal<-fixed-length-bytes Type.] vals)
  sexp: `(Enum ,@(map :sexp vals))
  .element?: (cut member <> vals)
  .vals@: (list->vector vals)
  .json@: (list->vector (map json-normalize vals))
  .length-in-bits: (integer-length (1- (vector-length .vals@)))
  .length-in-bytes: (n-bytes<-n-bits .length-in-bits)
  .<-nat: (cut vector-ref .vals@ <>)
  .nat<-: (lambda (v) (vector-index (cut equal? <> v) .vals@))
  .json<-: (lambda (v) (vector-ref .json@ (.nat<- v)))
  .<-json: (lambda (j) (vector-index (cut equal? <> j) .json@))
  .bytes<-: (compose (cut bytes<-nat <> .length-in-bytes) .nat<-)
  .<-bytes: (compose .<-nat nat<-bytes)
  .marshal: => (lambda (super) (if (zero? .length-in-bytes) void super))
  .unmarshal: => (lambda (super) (cond
                             ((< 0 .length-in-bytes) super)
                             ((null? vals) (lambda _ (error "no value to unmarshal of type" sexp)))
                             (else (let (val (car vals)) (lambda _ val)) super))))
(defrule (Enum values ...) {(:: @ Enum.) vals: '(values ...)})

(.def (Pair. @ [methods.bytes<-marshal Type.] left right)
  sexp: `(Pair ,(.@ left sexp) ,(.@ right sexp))
  .json<-: (lambda (v) [(json<- left (car v)) (json<- right (cdr v))])
  .<-json: (lambda (j) (cons (car j) (cadr j)))
  .marshal: (lambda (v port)
              (marshal left (car v) port)
              (marshal right (cdr v) port))
  .unmarshal: (lambda (port) (let* ((a (unmarshal left port))
                               (d (unmarshal right port)))
                          (cons a d))))
(def (Pair left right) {(:: @ Pair.) (left) (right)})

(.def (Maybe. @ [methods.bytes<-marshal Type.] type)
  sexp: `(Maybe ,(.@ type sexp))
  .element?: (lambda (x) (or (eq? x null) (element? type x)))
  .sexp<-: (lambda (v) (if (eq? v null) 'null (sexp<- type v)))
  .json<-: (lambda (v) (if (eq? v null) v ((.@ type .json<-) v)))
  .<-json: (lambda (j) (if (eq? j null) j ((.@ type .<-json) j)))
  .marshal: (λ (x port) (cond ((eq? x null) (write-byte 0 port))
                              (else (write-byte 1 port) (marshal type x port))))
  .unmarshal: (λ (port) (if (zero? (read-byte port)) null (unmarshal type port))))
(def (Maybe type) {(:: @ Maybe.) (type)})

(.def (Map. @ [methods.string&bytes&marshal<-json Type.] Key Value)
  sexp: `(Map ,(.@ Value sexp) <- ,(.@ Key sexp))
  .element?: (lambda (x) (and (hash-table? x)
                         (let/cc return
                           (hash-for-each (lambda (k v) (unless (and (element? Key k)
                                                                (element? Value v))
                                                     (return #f)))
                                          x) #t)))
  .empty: (make-hash-table)
  .json<-: (lambda (m) (hash-key-value-map m (cut string<- Key <>) (cut json<- Value <>)))
  .<-json: (lambda (j) (hash-key-value-map j (cut <-string Key <>) (cut <-json Value <>))))
(defrules Map (<- ->)
  ((_ Value <- Key) {(:: @ Map.) Key: Key Value: Value})
  ((_ Key -> Value) {(:: @ Map.) Key: Key Value: Value}))
