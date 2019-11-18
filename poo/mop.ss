;;-*- Gerbil -*-
;;; Classes on top of POO

(export #t)
(import :clan/utils/debug)

(import
  :gerbil/gambit/exact :gerbil/gambit/ports
  :std/format :std/iter :std/lazy :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/hash :clan/poo/poo)

;; TODO: use syntax-case instead, and have the prototype vs class thing be an option.
;; also an option to use a different name for the function and the slot.
(defrules .defgeneric () ;; define a generic function that invokes the prototype's slot
  ((_ (fun self args ...))
   ;; For now, just define the outer accessor :-/
   (def (fun self args ...) (.call self fun args ...))))

(defrules .defgeneric* () ;; define a generic function that invokes a slot in the .type slot
  ((_ (fun self args ...))
   ;; For now, just define the outer accessor :-/
   (def (fun self args ...) (.call (.@ self .type) fun self args ...))))

;; Slogan: A Type meta-object as a prototype is a class descriptor,
;; as an instance is a type descriptor.

;; TODO: add support for generation and simplification of test data,
;; in the style of hypothesis.works, Racket Redex, etc.
;; Maybe as monkey patching in another file?
;; Is there a theory for safe monkey patching of mutually-recursive data structures?

(.def (Type. @)
  (name (error "missing type name" @))
  (type-level 0)
  (element? (error "missing element?" @)))

(.defgeneric (element? type x))

(def (typecheck type x (msg #f))
  (assert! (element? type x)
           (format "~a type ~a: ~a" (or msg "not an element of") (.@ type name) (repr x))))

(def (validate type x (msg #f))
  (typecheck type x msg) x)

(.def (Any @ Type.)
  (name 'Any)
  (element? true))

(.def (Bool @ Type.)
  (name 'Bool)
  (element? boolean?))

(def (symbolify x) (!> x object->string string->symbol))

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

;; TODO: support optional and keyword arguments in the input types
(.def (Function. @ Type. output inputs)
  (name (symbolify `(Function ,(.@ output name) ,@(map (cut .@ <> name) inputs))))
  (element? procedure?) ;; we can't dynamically test that a function has the correct signature :-(
  (arity (length inputs)))

(def (Function output inputs)
  (typecheck Type output)
  (for-each (cut typecheck Type <>) inputs)
  (.o (:: @ Function.)
    (output) (inputs)))

(.def (Poo @ Type.)
  (name 'Poo)
  (element? poo?))

(.def (String @ Type.)
  (name 'String)
  (element? string?))

(.def (Symbol @ Type.)
  (name 'Symbol)
  (element? symbol?))

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

(.def (Integer @ (IntegerRange)) (name 'Integer))

(.def (Number @ Type.) (name 'Number) (element? number?))

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

(def (poo-values x)
  (map (λ (slot) (.ref x slot)) (.all-slots x)))

(.def (MonomorphicPoo. @ Type. type) ;; all the values are of given type
  (name (symbolify `(MonomorphicPoo ,(.@ type name))))
  (element? (λ (x) (and (poo? x) (every (element? type value) (poo-values x))))))

(def (MonomorphicPoo type) (.o (:: @ MonomorphicPoo.) (type)))
(def PooPoo (MonomorphicPoo Poo))
(def (map-poo-values f poo)
  (def m (.o))
  (for-each (λ (slot) (.put! m slot (f (.ref poo slot))))
            (.all-slots poo))
  m)

(.def (Pair. @ Type. left right)
  (name (symbolify `(Pair ,(.@ left name) ,(.@ right name)))))
(def (Pair left right) (.o (:: @ Pair.) (left) (right)))

(def (constant-slot x) (λ (_ _) x))

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
  (slots (.o (.type (.o (type Type) (default class) (hidden #t)))))
  (proto
   (let ((p (.o)))
     (for-each (λ (slot-name) (slot-definer (.ref effective-slots slot-name) slot-name p))
               (.all-slots effective-slots))
     p))
  (sealed #f))

(def ClassProto Class.)

(.def (Slot @ Class.)
  (name 'Slot)
  (slots
   (.o
    (type (.o (type Type) (optional #t)))
    (constant (.o (type Any) (optional #t)))
    (compute (.o (type (Function Any Poo)) (optional #t)))
    (default (.o (type Any) (optional #t)))
    (optional (.o (type Bool) (default #f)))
    (hidden (.o (type Bool) (default #f)))))
  (proto (.o (.type @) (optional #f) (hidden #f)))
  (slot-checker
   (λ (@@ slot-name x)
     (with-slots (@@ type constant optional)
       (if (.key? x slot-name)
         (let ((value (.ref x slot-name)))
           (and
             (or (not (.has? @@ type)) (element? type value))
             (or (not (.has? @@ constant)) (equal? constant value))))
         (and (.has? @@ optional) optional)))))
  (slot-definer
   (λ (@@ slot-name x)
     (with-slots (@@ type constant compute default)
       (cond
        ((.has? @@ constant) (.putslot! x slot-name (constant-slot constant)))
        ((.has? @@ compute) (.putslot! x slot-name compute))
        ((.has? @@ default) (.putslot! x slot-name (constant-slot default)))
        ((and (.has? @@ type) (.has? type proto))
         (.putslot! x slot-name (constant-slot (.@ type proto)))))
       ;;TODO: (put-assertion! x (λ (self) (assert! (slot-checker slot-name self))))
       ))))

(.def (Type @ Class.)
  (name 'Type)
  (type-level 1)
  (slots
   (.o
    (name (.o (type Symbol)))
    (type-level (.o (type Integer)))
    (element? (.o (type (Function Bool Any))))))
  (element? (λ (x) (and (poo? x) (.has? x name) (.has? x element?))))
  (proto Type.))

(.def (Class @ Type)
   (name 'Class)
   (type-level 2)
   (slot-descriptor-class Slot) ;; MOP magic!
   (slots =>.+
    (.o
     (slots (.o (type PooPoo))) ;; would be (MonomorphicPoo Slot) if we didn't automatically append Slot
     (sealed (.o (type Bool) (default #f)))))
   (proto Class.))

(def (proto class) (.@ class proto))

;; TODO: make-instance or new should .instantiate the object.
;; TODO: What name for a syntax that does not instantiate it?
(defrules new ()
  ((_ (class self slots ...) slot-defs ...)
   (.o (:: self (proto class) slots ...) slot-defs ...))
  ((_ (class) slot-defs ...)
   (.o (:: self (proto class)) slot-defs ...))
  ((_ class slot-defs ...)
   (.o (:: self (proto class)) slot-defs ...)))
