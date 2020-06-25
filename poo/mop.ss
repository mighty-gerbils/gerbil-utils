;;-*- Gerbil -*-
;;; Classes on top of POO
;; Slogan: A Type meta-object as a prototype is a class descriptor,
;; as an instance is a type descriptor.

(export #t)

(import
  (for-syntax :std/srfi/1)
  :gerbil/gambit/exact :gerbil/gambit/hash :gerbil/gambit/ports
  :std/format :std/generic :std/iter :std/lazy
  :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  ../utils/base ../utils/hash ./poo ./brace)

;;TODO: Parse Gerbil Scheme formals, extract call arguments
(begin-syntax
  (def (stx-keyword? x) (keyword? (syntax-e x)))
  (def (formal->variable f)
    (syntax-case f ()
      (_ (identifier? f) f)
      ((v _) (identifier? #'v) #'v)
      (_ (error "Invalid formal parameter" f))))
  (def (formals->variables formals)
    (let loop ((rvars [])
               (formals formals))
      (syntax-case formals ()
        (() (reverse rvars))
        ((k v . rest) (stx-keyword? #'k) (loop (cons (formal->variable #'v) rvars) #'rest))
        ((v . rest) (loop (cons (formal->variable #'v) rvars) #'rest))
        (_ (error "Invalid formals" formals)))))
  (def (formals->call formals)
    (let loop ((rcall [])
               (formals formals))
      (syntax-case formals ()
        (() (reverse rcall))
        ((k v . rest) (stx-keyword? #'k) (loop [(formal->variable #'v) #'k . rcall] #'rest))
        ((v . rest) (loop [(formal->variable #'v) . rcall] #'rest))
        (_ (error "Invalid formals" formals))))))

;; * Options
;; default: default value when no method is defined, also bottom value for fixed-point
;; from: type specifies that the methods will be provided by the methods of the .type field.
;; slot: slot-name can specify an alternate slot-name, which helps avoid with-slot shadowing
;;   when calling the gf from a method definition.
;; TODO: add method combination, which requires integration with the recursion in .ref.
;;   at some point, the information about the slot needs to be made available in the meta object.
;; TODO: make this information somehow available at compile-time when possible,
;;   to enable inlining and partial evaluation.
(defsyntax (.defgeneric stx) ;; define a generic function that invokes the prototype's slot
  (def (parse-formals formals)
    (syntax-case formals ()
      ((fun self args ...) (values #'fun #'self #'(args ...)))
      (fun (identifier? #'fun) (values #'fun #'self #f))
      (_ (error "bad formals for .defgeneric" stx))))
  (def (parse-options options)
    (def slot-name #f)
    (def default #f)
    (def from 'instance)
    (let loop ((options options))
      (match options
        ([] (values slot-name default from))
        ([kw val . rest]
         (match (syntax-e kw)
           (slot: (set! slot-name val))
           (from: (case (syntax-e val)
                    ((type) (set! from 'type))
                    ((methods) (set! from 'methods))
                    ((instance) (set! from 'instance))
                    (else (error "invalid option" val))))
           (default: (set! default val))
           (x (error "invalid option" x options)))
         (loop rest))
        (_ (error "invalid options" options (syntax->datum options))))))
  (syntax-case stx ()
    ((.defgeneric formals options ...)
     (begin
       (define-values (fun self funargs) (parse-formals #'formals))
       (define-values (slot-name default from) (parse-options (syntax->list #'(options ...))))
       (with-syntax* ((gf fun)
                      (self self)
                      ((args ...) (or funargs #'()))
                      ((vars ...) (formals->variables #'(args ...)))
                      (slot-name (or slot-name fun))
                      (default* default)
                      (base (if default #'(λ () default*) #'(no-such-slot self 'slot-name)))
                      (methods (case from
                                 ((instance) #'self)
                                 ((methods) #'(.get self methods))
                                 ((type) #'(.get self .type methods))
                                 (else (error "invalid from" (syntax->datum stx)))))
                      (getter #'(.ref methods 'slot-name base))
                      ((evars ...) (case from
                                     ((type) #'(self vars ...))
                                     ((instance methods) #'(vars ...))
                                     (else (error "invalid from" (syntax->datum stx))))))
         (if (or funargs (case from ((type) #t) ((instance methods) #f)))
           #'(def (gf self args ...) (getter evars ...))
           #'(def (gf self) getter)))))))

;;(def (and-combination new-value old-value) (and (new-value dont-call-next-method) (old-value)))

(defrules .method ()
  ((_ poo slot) (.get poo methods slot))
  ((_ poo slot args ...) ((.method poo slot) args ...)))

(defrules .type.method ()
  ((_ poo slot) (.get poo .type methods slot))
  ((_ poo slot args ...) ((.type.method poo slot) args ...)))

(.defgeneric (element? type x)
   ;;default: false
   ;;combination: and-combination
   slot: .element?)

(.def (Type. @)
  sexp: (error "missing type sexp" @)
  .element?: (error "missing element?" @)
  methods: {
  })

(def (typecheck type x (msg #f))
  (assert! (element? type x)
           (format "~a type ~a: ~a"
                   (or msg "not an element of") (.@ type sexp) (repr x))))

(def (validate type x (msg #f)) (typecheck type x msg) x)

(def (poo-values x) (map (cut .ref x <>) (.all-slots x)))
(def (symbolify x) (!> x object->string string->symbol))

(.def (Any @ Type.) sexp: 'Any .element?: true)
(.def (Poo @ Type.) sexp: 'Poo .element?: poo?)
(.def (Bool @ Type.) sexp: 'Bool .element?: boolean?)

(def (monomorphic-poo? type x)
  (and (poo? x) (every (cut element? type <>) (poo-values x))))

(.def (MonomorphicPoo. @ Type. type) ;; all the values are of given type
  sexp: `(MonomorphicPoo ,(.@ type sexp))
  .element?: (cut monomorphic-poo? type <>))

(def (MonomorphicPoo type) {(:: @ MonomorphicPoo.) (type)})
(def PooPoo (MonomorphicPoo Poo))
(def (map-poo-values f poo)
  (def m {})
  (for-each (λ (slot) (.put! m slot (f (.ref poo slot))))
            (.all-slots poo))
  m)

;; TODO: support optional and keyword arguments in the input types
(.def (Function. @ Type. outputs inputs)
  sexp: `(Function (@list ,@(map (cut .@ <> sexp) outputs)) (@list ,@(map (cut .@ <> sexp) inputs)))
  .element?: procedure? ;; we can't dynamically test that a function has the correct signature :-(
  arity: (length inputs))

(def (Function outputs inputs)
  (for-each (cut typecheck Type <>) outputs)
  (for-each (cut typecheck Type <>) inputs)
  { (:: @ Function.) (outputs) (inputs) })

;; The expander complains "Syntax Error: Ambiguous pattern".
;; TODO: Use syntax-case, detect when there are opposite arrows, curry when there are multiple ones?
(defsyntax (Fun stx)
  (syntax-case stx (<- ->)
    ((_ . io)
     (let (iol (syntax->list #'io))
       (cond
        ((list-index (lambda (x) (eq? (stx-e x) '<-)) iol)
         => (lambda (k)
              (defvalues (outputs inputs) (split-at iol k))
              (let loop ((o outputs) (i (cdr inputs)))
                (cond
                 ((list-index (lambda (x) (eq? (stx-e x) '<-)) i)
                  => (lambda (k)
                       (defvalues (inputs moreinputs) (split-at i k))
                       (loop [[#'Function [#'@list . o] [#'@list . inputs]]] (cdr moreinputs))))
                 (else [#'Function [#'@list . o] [#'@list i]])))))
        ((list-index (lambda (x) (eq? (stx-e x) '->)) iol)
         => (lambda (k)
              (defvalues (inputs ios) (split-at iol k))
              (let loop ((i inputs) (iol (cdr ios)))
                (cond
                 ((list-index (lambda (x) (eq? (stx-e x) '->)) i)
                  => (lambda (k)
                       (defvalues (inputs moreios) (split-at i k))
                       [#'Function [#'@list (loop inputs (cdr moreios))] [#'@list . i]]))
                 (else [#'Function [#'@list . iol] [#'@list . i]])))))
        (else (error "illegal Fun type" stx))))))) ;; or (Values . io) ?

(.defgeneric (slot-checker slot-descriptor slot-name base x) slot: .slot-checker from: type)
(.defgeneric (slot-definer slot-descriptor slot-name x) slot: .slot-definer from: type)

(.def (Class. class Type. slots sexp sealed) ;; this is the class descriptor for class descriptor objects.
  .type: Class
  effective-slots:
   (let (slot-base (.@ .type slot-descriptor-class proto))
     (map-poo-values (cut .mix <> slot-base) slots))
  .element?:
   (λ (x)
     (and (poo? x)
          (every (λ (slot-name)
                   (def slot (.ref effective-slots slot-name))
                   (def base (.ref slot 'base (λ () (no-such-slot x slot-name))))
                   (slot-checker slot slot-name base x))
                 (.all-slots effective-slots))
          (or (not sealed) ;; sealed means only defined slots can be present.
              (every (cut .key? effective-slots <>) (.all-slots x)))))
  slots: {.type: {type: Type default: class hidden: #t}}
  proto:
   (let ((p {}))
     (for-each (λ (slot-name)
                 (def slot (.ref effective-slots slot-name))
                 (slot-definer slot slot-name p))
               (.all-slots effective-slots))
     p)
  sealed: #f)

(def ClassProto Class.)

(def (constant-slot x) (λ (_ _ _) x))

(.def (Slot @ Class.)
  sexp: 'Slot
  slots:
   {type: {type: Type optional: #t}
    constant: {type: Any optional: #t}
    compute: {type: (Fun Any <- Poo Any (Fun Any <-)) optional: #t} ;; second Any should be (List Poo)
    base: {type: Any optional: #t default: no-such-slot}
    default: {type: Any optional: #t}
    optional: {type: Bool default: #f}
    hidden: {type: Bool default: #f}}
  proto: {.type: @ optional: #f hidden: #f}
  methods:
  {.slot-checker:
    (λ (@@ slot-name base x)
      (with-slots (@@ type constant optional)
        (if (.key? x slot-name)
          (let ((value (.ref x slot-name base)))
            (and
              (or (not (.has? @@ type)) (element? type value))
              (or (not (.has? @@ constant)) (equal? constant value))))
          (and (.has? @@ optional) optional))))
   .slot-definer:
    (λ (@@ slot-name x)
       (with-slots (@@ type constant compute default)
         (cond
          ((.has? @@ constant) (.putslot! x slot-name (constant-slot constant)))
          ((.has? @@ compute) (.putslot! x slot-name compute))
          ((.has? @@ default) (.putslot! x slot-name (constant-slot default)))
          ((and (.has? @@ type) (.has? type proto))
           (.putslot! x slot-name (constant-slot (.@ type proto)))))
         ;;TODO: (put-assertion! x (λ (self) (assert! (slot-checker slot-name base self))))
         ))})

;; TODO: functional lenses in (methods .lens foo) as well as imperative accessors
(.def (Lens @ Class.)
  sexp: 'Lens. ;; Lens 's 'a
  slots: =>.+ { ;; or should we have just a ((f a) <- (f : Functor) ((f b) <- b) a) ?
    .get: { } ;; 's -> 'a
    .set: { }} ;; 'a -> 's -> 's
  methods: =>.+ {
    .get: (lambda (l s) (.call l .get s))
    .set: (lambda (l a s) (.call l .set a s))
    .modify: (lambda (l f s)
               (.call l .set (f (.call l .get s)) s)) ;; over in haskell

    ;; Same order as in Haskell, opposite to OCaml. (compose x y) will access x then y
    ;; (Lens 'a 'c)  <- (Lens 'a 'b) (Lens 'b 'c)
    .compose: (lambda (l1 l2) {.get: (lambda (s) (.call l2 .get (.call l1 s)))
                          .set: (lambda (a s) (.modify l1 (cut .call l2 .set a <>) s))})})

(.def (Type @ Class.)
  sexp: 'Type
  slots: {sexp: {type: Any}
          .element?: {type: (Fun Bool <- Any)}}
  .element?: (λ (x) (and (poo? x) (.has? x sexp) (.has? x .element?)))
  proto: Type.)

(.def (Class @ Type)
   sexp: 'Class
   slot-descriptor-class: Slot ;; MOP magic!
   slots: =>.+
    {slots: {type: PooPoo} ;; would be (MonomorphicPoo Slot) if we didn't automatically append Slot
     sealed: {type: Bool default: #f}}
   methods: =>.+
    {.sexp<-: (lambda (x) (.@ x sexp))}
   proto: Class.)

(def (proto class) (.@ class proto))

;; TODO: make-instance or new should .instantiate the object.
;; TODO: What name for a syntax that does not instantiate it?
(defrules new ()
  ((_ (class self slots ...) slot-defs ...)
   {(:: self (proto class) slots ...) slot-defs ...})
  ((_ (class) slot-defs ...)
   {(:: self (proto class)) slot-defs ...})
  ((_ class slot-defs ...)
   {(:: self (proto class)) slot-defs ...}))

(defrules .defclass ()
  ((_ (class class-options ...) (slotdefs ...) options ...)
   (.def (class class-options ...) sexp: 'class (slots =>.+ {slotdefs ...}) options ...))
  ((_ class (slotdefs ...) options ...)
   (.defclass (class) (slotdefs ...) options ...)))

(defmethod (@@method :pr poo)
  (λ (self (port (current-output-port)) (options (current-representation-options)))
    (cond
     ((.has? self .type print-object) (.call (.@ self .type) print-object self port options))
     ((.has? self .type methods .sexp<-) (write (sexp<- (.@ self .type) self) port))
     ((.has? self .type) (print-class-object self port options))
     ((.has? self :pr) (.call self :pr port options))
     ((.has? self sexp) (write (.@ self sexp) port))
     (else (print-unrepresentable-object self port options)))))

(.defgeneric (sexp<- type x) slot: .sexp<- from: methods)

(defgeneric :sexp
  (lambda (x)
    (cond
     ((or (number? x) (boolean? x) (string? x) (char? x) (void? x) (keyword? x) (eof-object? x))
      x)
     (else `',x)))) ;; TODO: do better than that.


(defmethod (@@method :sexp poo)
  (λ (self)
    (cond
     ((.has? self .type methods .sexp<-) (.call (.@ self .type methods) .sexp<- self))
     ((.has? self sexp) (object->string (.@ self sexp))))))

(def (print-class-object
      x (port (current-output-port)) (options (current-representation-options)))
  (def (d x) (display x port))
  (def (w x) (write x port))
  (d "(begin0 #") (d (object->serial-number x)) (d " {")
  (try
   (for-each (λ-match ([k . v] (d " (") (w k) (d " ") (prn v) (d ")"))) (.alist x))
   (catch (e) (void)))
  (d "})"))

(def (slot-lens slot-name)
  {(:: @ (proto Lens))
   .get: (lambda (s) (.ref s slot-name))
   .set: (lambda (x s) (.cc s slot-name x))})
