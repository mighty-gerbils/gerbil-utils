;;-*- Gerbil -*-
;;; Classes on top of POO
;; Slogan: A Type meta-object as a prototype is a class descriptor,
;; as an instance is a type descriptor.

(export #t)

(import
  :gerbil/gambit/exact :gerbil/gambit/hash :gerbil/gambit/ports
  :std/format :std/iter :std/lazy :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/hash :clan/poo/poo :clan/poo/brace)

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
  repr: (error "missing type repr" @)
  .element?: (error "missing element?" @)
  methods: {})

(def (typecheck type x (msg #f))
  (assert! (element? type x)
           (format "~a type ~a: ~a"
                   (or msg "not an element of") (.@ type name) (repr x))))

(def (validate type x (msg #f)) (typecheck type x msg) x)

(def (poo-values x) (map (cut .ref x <>) (.all-slots x)))
(def (symbolify x) (!> x object->string string->symbol))

(.def (Any @ Type.) repr: 'Any .element?: true)
(.def (Poo @ Type.) repr: 'Poo .element?: poo?)
(.def (Bool @ Type.) repr: 'Bool .element?: boolean?)

(def (monomorphic-poo? type x)
  (and (poo? x) (every (cut element? type <>) (poo-values x))))

(.def (MonomorphicPoo. @ Type. type) ;; all the values are of given type
  repr: `(MonomorphicPoo ,(.@ type repr))
  .element?: (cut monomorphic-poo? type <>))

(def (MonomorphicPoo type) {(:: @ MonomorphicPoo.) (type)})
(def PooPoo (MonomorphicPoo Poo))
(def (map-poo-values f poo)
  (def m {})
  (for-each (λ (slot) (.put! m slot (f (.ref poo slot))))
            (.all-slots poo))
  m)

;; TODO: support optional and keyword arguments in the input types
(.def (Function. @ Type. output inputs)
  repr: `(Function ,(.@ output name) ,@(map (cut .@ <> name) inputs))
  .element?: procedure? ;; we can't dynamically test that a function has the correct signature :-(
  arity: (length inputs))

(def (Function output inputs)
  (typecheck Type output)
  (for-each (cut typecheck Type <>) inputs)
  { (:: @ Function.) (output) (inputs) })

(.defgeneric (slot-checker slot-descriptor slot-name base x) slot: .slot-checker from: type)
(.defgeneric (slot-definer slot-descriptor slot-name x) slot: .slot-definer from: type)

(.def (Class. class Type. slots name sealed) ;; this is the class descriptor for class descriptor objects.
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
  repr: 'Slot
  slots:
   {type: {type: Type optional: #t}
    constant: {type: Any optional: #t}
    compute: {type: (Function Any [Poo Any (Function Any [])]) optional: #t} ;; second Any should be (List Poo)
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

(.def (Type @ Class.)
  repr: 'Type
  slots: {repr: {type: Any}
          .element?: {type: (Function Bool Any)}}
  .element?: (λ (x) (and (poo? x) (.has? x repr) (.has? x .element?)))
  proto: Type.)

(.def (Class @ Type)
   repr: 'Class
   slot-descriptor-class: Slot ;; MOP magic!
   (slots =>.+
    {slots: {type: PooPoo} ;; would be (MonomorphicPoo Slot) if we didn't automatically append Slot
     sealed: {type: Bool default: #f}})
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
   (.def (class class-options ...) repr: 'class (slots =>.+ {slotdefs ...}) options ...))
  ((_ class (slotdefs ...) options ...)
   (.defclass (class) (slotdefs ...) options ...)))

(defmethod (@@method :pr poo)
  (λ (self (port (current-output-port)) (options (current-representation-options)))
    (cond
     ((.has? self :pr) (.call self :pr port options))
     ((.has? self .type print-object) (.call (.@ self .type) print-object port options))
     ((.has? self .type) (print-class-object self port options)))))

(def (print-class-object
      x (port (current-output-port)) (options (current-representation-options)))
  (def (d x) (display x port))
  (def (w x) (write x port))
  (d "(begin0 #") (d (object->serial-number x)) (d " {")
  (try
   (for-each (λ-match ([k . v] (d " (") (w k) (d " ") (prn v) (d ")"))) (.alist x))
   (catch (e) (void)))
  (d "})"))
