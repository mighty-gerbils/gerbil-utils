;;-*- Gerbil -*-
;;; Classes on top of POO

(export
  .defgeneric)

(import
  :gerbil/gambit/exact :gerbil/gambit/ports
  :std/format :std/iter :std/lazy :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/hash :clan/poo/poo)

(defrules .defgeneric ()
  ((_ (fun self args ...))
   ;; For now, just define the outer accessor :-/
   (def (fun self args ...) (.call self fun args ...))))

;; Slogan: A Type meta-object as a prototype is a class descriptor,
;; as an instance is a type descriptor.

;; TODO: add support for generation and simplification of test data,
;; in the style of hypothesis.works, Racket Redex, etc.
;; Maybe as monkey patching in another file?
;; Is there a theory for safe monkey patching of mutually-recursive data structures?

(.def Type.
  (name (error "missing type name"))
  (type-level 0)
  (element? (error "missing element?")))

(.def Type
  (.type Class)
  (type-level 1)
  (name 'Type)
  (slots
   (.o
    (name (.o (type Symbol)))
    (type-level (.o (type Integer)))
    (element? (.o (type (Function Bool Any))))))
  (proto Type.))

(.defgeneric (element? type x))

(def (typecheck type x (msg #f))
  (assert! (element? type x)
           (format "~a type ~a: ~a" (or msg "not an element of") (.get type name) (repr x))))

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
  (name (symbolify `(Tuple ,@(map (cut .get <> name) (vector->list types)))))
  (element?
    (λ (x)
      (def l (vector-length types))
      (and (vector? x) (= (vector-length x) l)
           (let/cc return
             (for ((i (in-iota l)))
               (unless (element? (vector-ref types i) (vector-ref x i)) (return #f)))
             #t)))))

(def (Tuple . types) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (validate Type types))))
  (.o (:: @ Tuple.) (types)))

;; TODO: support optional and keyword arguments in the input types
(.def (Function. @ Type. output inputs)
  (name (symbolify `(Function ,(.get output name) ,@(map (cut .get <> name) inputs))))
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

(.def (IntegerRange. @ Type.)
  (name (symbolify `(IntegerRange ,@(if min `(min: ,min) '()) ,@(if max `(max: ,max) '()))))
  (element?
   (match (vector min max)
     ((vector #f #f) exact-integer?)
     ((vector min #f) (λ (x) (and (exact-integer? x) (<= min x))))
     ((vector #f max) (λ (x) (and (exact-integer? x) (<= x max))))
     ((vector min max) (λ (x) (and (exact-integer? x) (<= min x max)))))))

(def (IntegerRange min: (min #f) max: (max #f))
  (assert! (or (not min) (exact-integer? min)))
  (assert! (or (not max) (exact-integer? max)))
  (.o (:: @ IntegerRange.) (min) (max)))

(.def (Integer @ (IntegerRange)) (name 'Integer))

(.def (Number @ Type.) (name 'Number) (element? number?))

(.def (List. @ Type. type)
  (name (symbolify `(List ,(.get type name))))
  (element? (λ (x) (and (list? x) (every (cut element? type <>) x)))))

(def (List type)
  (typecheck Type type)
  (.o (:: @ List.) (type)))

(.def (Or. @ Type. types)
  (name (symbolify `(Or ,@(map (cut .get <> name) types))))
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
  (name (symbolify `(MonomorphicPoo ,(.get type name))))
  (element? (λ (x) (and (poo? x) (every (element? type value) (poo-values x))))))

(def (MonomorphicPoo type) (.o (:: @ MonomorphicPoo.) (type)))
(def PooPoo (MonomorphicPoo Poo))
(def (map-poo-values f poo)
  (def m (.o))
  (for-each (λ (slot) (.put! m slot (f (.ref poo slot)))) (.all-slots poo))
  m)

(.def (Pair. @ Type. left right)
  (name (symbolify `(Pair ,(.get left name) ,(.get right name)))))
(def (Pair left right) (.o (:: @ Pair.) (left) (right)))

(.def (SlotDescriptor Descriptor Class)
  (slots
    (.o
     (slots
      (.o
       (type (.o (type Type) (optional #t)))
       (constant (.o (type Any) (optional #t)))
       (compute (.o (type (Function Any Poo)) (optional #t)))
       (default (.o (type Any) (optional #t)))
       (optional (.o (type Bool) (default #f)))
       (hidden (.o (type Bool) (default #f)))))
     (proto
      (.o (:: @ [] type constant compute default optional hidden)
        (.type Descriptor)
        (slot-checker ;; TODO: partially evaluate based on slot metadata
         (λ (slot-name x)
           (if (not (.has? x slot-name)) optional
               (let ((value (.ref x slot-name)))
                 (and
                   (or (not (.has? @ 'type)) (element? type value))
                   (or (not (.has? @ 'constant) (equal? value constant))))))))
        (slot-definer ;;
         (λ (slot-name x)
           (cond
            ((.has? @ 'constant) (.putslot! x slot-name (λ (_ _) constant)))
            ((.has? @ 'compute) (.putslot! x slot-name compute))
            ((.has? @ 'default) (.putslot! x slot-name (λ (_ _) default)))
            ((and (.has? @ 'type) (.has? type 'proto))
             (.putslot! x slot-name (λ (_ _) (.get type proto)))))
           ;;TODO: (put-assertion! x (λ (self) (assert! (slot-checker slot-name self))))
           )))))))

(.def (Class. @ Type. slots name sealed) ;; this is the class descriptor for class descriptor objects.
   (.type Class)
   (effective-slots (map-poo-values (λ (slot) (.mix slot (.get SlotDescriptor proto)))))
   (element?
    (λ (x)
      (and (poo? x)
           (every (λ (slot-name) ((.get (.ref effective-slots slot-name) slot-checker) slot-name x))
                  (.all-slots x))
           (or (not sealed) ;; sealed means only defined slots can be present.
               (every (cut .has? effective-slots <>) (.all-slots x))))))
   (proto
    (let ((p (.o)))
      (for-each (λ (slot-name) (.call (.ref effective-slots slot-name) slot-definer slot-name p))
                (.all-slots effective-slots))
      p)))

(.def (Class @ Class.)
   (name 'Class)
   (slots
    (.o
     (name (.o (type Symbol)))
     (slots (.o (type PooPoo))) ;; would be (MonomorphicPoo Slot) if we didn't automatically append Slot
     (sealed (.o (type Bool) (default #f)))))
   (proto Class.))
