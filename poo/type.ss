;;-*- Gerbil -*-
;;; More types on top of POO and its MOP

(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/format :std/iter :std/lazy :std/misc/list :std/srfi/1 :std/sugar
  ../utils/base ../utils/hash ../utils/io ../utils/maybe
  ./poo ./mop ./brace ./io ./number)

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

(.def (List. @ Type. type)
  sexp: `(List ,(.@ type name))
  .element?: (λ (x) (and (list? x) (every (cut element? type <>) x)))
  methods: {(:: methods [bytes<-un/marshal])
    .sexp<-: (lambda (v) ['@list (map (.@ type methods .sexp<-) v) ...])
    .json<-: (lambda (v) (map (.@ type methods .json<-) v))
    .<-json: (lambda (j) (map (.@ type methods .<-json) j))
    .marshal: (let (m (.@ type methods .marshal))
                (lambda (v port) (write-uint16 (length v) port) (for-each (cut m <> port) v)))
    .unmarshal: (let (u (.@ type methods .unmarshal))
                  (lambda (port) (def l (read-uint16 port))
                     (for/collect (_ (in-range l)) (u port))))})
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
  sexp: `(Pair ,(.@ left sexp) ,(.@ right sexp))
  methods: =>.+ {(:: @ [bytes<-un/marshal])
    .json<-: (lambda (v) [(json<- left (car v)) (json<- right (cdr v))])
    .<-json: (lambda (j) (cons (car j) (cadr j)))
    .marshal: (lambda (v port)
                (marshal left (car v) port)
                (marshal right (cdr v) port))
    .unmarshal: (lambda (port) (let* ((a (unmarshal left port))
                                 (d (unmarshal right port)))
                            (cons a d)))
    })
(def (Pair left right) {(:: @ Pair.) (left) (right)})

(.def (Maybe. @ Type. type)
  sexp: `(Maybe ,(.@ type sexp))
  .element?: (lambda (x) (or (eq? x null) (element? type x)))
  methods: {(:: @@ [bytes<-un/marshal])
    .sexp<-: (lambda (v) (if (eq? v null) 'null (sexp<- type v)))
    .json<-: (lambda (v) (if (eq? v null) v ((.@ type methods .json<-) v)))
    .<-json: (lambda (j) (if (eq? j null) j ((.@ type methods .<-json) j)))
    .marshal: (λ (x port) (cond ((eq? x null) (write-byte 0 port))
                                (else (write-byte 1 port) (marshal type x port))))
    .unmarshal: (λ (port) (if (zero? (read-byte port)) null (unmarshal type port)))
})
(def (Maybe type) {(:: @ Maybe.) (type)})

(.def (Symbol @ Type.) sexp: 'Symbol .element?: symbol?)
(.def (String @ Type.) sexp: 'String .element?: string?)
