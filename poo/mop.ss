;;-*- Gerbil -*-
;;; Classes on top of POO
;; Slogan: A Type meta-object as a prototype is a class descriptor,
;; as an instance is a type descriptor.

(export #t)

(import
  (prefix-in (only-in <MOP> @method) @)
  :gerbil/gambit/exact :gerbil/gambit/hash :gerbil/gambit/ports
  :std/format :std/iter :std/lazy :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/hash :clan/poo/poo :clan/poo/brace)

;; TODO: use syntax-case instead, and have the prototype vs class thing be an option.
;; also an option to use a different name for the function and the slot.
(defrules .defgeneric () ;; define a generic function that invokes the prototype's slot
  ((_ (fun self args ...))
   ;; For now, just define the outer accessor :-/
   (def (fun self args ...) (.call self fun args ...)))
  ((_ (fun self args ...) default)
   (def (fun self args ...)
     (if (.has? self fun) (.call self fun args ...) (default self args ...)))))

(defrules .defgeneric* () ;; define a generic function that invokes a slot in the .type slot
  ((_ (fun self args ...))
   ;; For now, just define the outer accessor :-/
   (def (fun self args ...) (.call (.@ self .type methods) fun self args ...))))

(.def (Type. @)
  name: (error "missing type name" @)
  element?: (error "missing element?" @))

(.defgeneric (element? type x))

(def (typecheck type x (msg #f))
  (assert! (element? type x)
           (format "~a type ~a: ~a"
                   (or msg "not an element of") (.@ type name) (repr x))))

(def (validate type x (msg #f)) (typecheck type x msg) x)

(def (poo-values x) (map (cut .ref x <>) (.all-slots x)))
(def (symbolify x) (!> x object->string string->symbol))

(.def (Any @ Type.) (name 'Any) (element? true))
(.def (Bool @ Type.) (name 'Bool) (element? boolean?))
(.def (Symbol @ Type.) (name 'Symbol) (element? symbol?))
(.def (Poo @ Type.) (name 'Poo) (element? poo?))
(.def (String @ Type.) (name 'String) (element? string?))
(.def (Integer @ Type.) (name 'Integer) (element? exact-integer?))
(.def (Number @ Type.) (name 'Number) (element? number?))

(def (monomorphic-poo? type x)
  (and (poo? x) (every (cut element? type <>) (poo-values x))))

(.def (MonomorphicPoo. @ Type. type) ;; all the values are of given type
  name: (symbolify `(MonomorphicPoo ,(.@ type name)))
  element?: (cut monomorphic-poo? type <>))

(def (MonomorphicPoo type) {(:: @ MonomorphicPoo.) (type)})
(def PooPoo (MonomorphicPoo Poo))
(def (map-poo-values f poo)
  (def m {})
  (for-each (λ (slot) (.put! m slot (f (.ref poo slot))))
            (.all-slots poo))
  m)

;; TODO: support optional and keyword arguments in the input types
(.def (Function. @ Type. output inputs)
  (name (symbolify `(Function ,(.@ output name) ,@(map (cut .@ <> name) inputs))))
  (element? procedure?) ;; we can't dynamically test that a function has the correct signature :-(
  (arity (length inputs)))

(def (Function output inputs)
  (typecheck Type output)
  (for-each (cut typecheck Type <>) inputs)
  { (:: @ Function.) (output) (inputs) })

(.defgeneric* (slot-checker slot-descriptor slot-name x))
(.defgeneric* (slot-definer slot-descriptor slot-name x))

(.def (Class. class Type. slots name sealed) ;; this is the class descriptor for class descriptor objects.
  (.type Class)
  (effective-slots
   (let (slot-base (.@ .type slot-descriptor-class proto))
     (map-poo-values (cut .mix <> slot-base) slots)))
  (element?
   (λ (x)
     (and (poo? x)
          (every (λ (slot-name) (slot-checker (.ref effective-slots slot-name) slot-name x))
                 (.all-slots effective-slots))
          (or (not sealed) ;; sealed means only defined slots can be present.
              (every (cut .key? effective-slots <>) (.all-slots x))))))
  (slots {.type: {type: Type default: class hidden: #t}})
  (proto
   (let ((p {}))
     (for-each (λ (slot-name) (slot-definer (.ref effective-slots slot-name) slot-name p))
               (.all-slots effective-slots))
     p))
  (sealed #f))

(def ClassProto Class.)

(def (constant-slot x) (λ (_ _) x))

(.def (Slot @ Class.)
  name: 'Slot
  slots:
   {type: {type: Type optional: #t}
    constant: {type: Any optional: #t}
    compute: {type: (Function Any Poo) optional: #t}
    default: {type: Any optional: #t}
    optional: {type: Bool default: #f}
    hidden: {type: Bool default: #f}}
  proto: {.type: @ optional: #f hidden: #f}
  methods:
  {slot-checker:
    (λ (@@ slot-name x)
      (with-slots (@@ type constant optional)
        (if (.key? x slot-name)
          (let ((value (.ref x slot-name)))
            (and
              (or (not (.has? @@ type)) (element? type value))
              (or (not (.has? @@ constant)) (equal? constant value))))
          (and (.has? @@ optional) optional))))
   slot-definer:
    (λ (@@ slot-name x)
       (with-slots (@@ type constant compute default)
         (cond
          ((.has? @@ constant) (.putslot! x slot-name (constant-slot constant)))
          ((.has? @@ compute) (.putslot! x slot-name compute))
          ((.has? @@ default) (.putslot! x slot-name (constant-slot default)))
          ((and (.has? @@ type) (.has? type proto))
           (.putslot! x slot-name (constant-slot (.@ type proto)))))
         ;;TODO: (put-assertion! x (λ (self) (assert! (slot-checker slot-name self))))
         ))})

(.def (Type @ Class.)
  name: 'Type
  slots: {name: {type: Symbol}
          element?: {type: (Function Bool Any)}}
  element?: (λ (x) (and (poo? x) (.has? x name) (.has? x element?)))
  proto: Type.)

(.def (Class @ Type)
   name: 'Class
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
   (.def (class class-options ...) name: 'class (slots =>.+ {slotdefs ...}) options ...))
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
