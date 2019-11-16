;;-*- Gerbil -*-
;; Trivial implementation of Prototype Object Orientation in Gerbil Scheme.
;;
;; See poo.md for documentation
;; TODO: see Future Features and the Internals TODO sections in document above.

(export
  poo defpoo poo? .mix .ref .instantiate .get .call .set! .def .has? .all-slots)

(import
  :std/format :std/lazy :std/misc/list :std/misc/rbtree :std/misc/repr
  :std/srfi/1
  :std/sugar
  :clan/utils/base :clan/utils/hash :clan/utils/list)

(defstruct Poo (prototypes instance))

(def (.instantiate poo)
  (match poo
    ((Poo _ #f) (set! (Poo-instance poo) (hash))) ;; TODO: call .init method?
    ((Poo _ _) (void)) ;; already instantiated
    (else (error "No poo" poo))))

(def (.ref poo slot)
  (.instantiate poo)
  (match poo
    ((Poo prototypes instance)
     (hash-ensure-ref instance slot (cut compute-slot poo prototypes slot)))))

(def (compute-slot poo prototypes slot)
  (match prototypes
    ([] (error "No such slot" poo slot))
    ([prototype . super-prototypes]
     (if-let (fun (hash-get prototype slot))
        (fun poo super-prototypes)
        (compute-slot poo super-prototypes slot)))))

(def (append-prototypes x (prototypes []))
  (match x ;; TODO: use lazy merging of patricia trees to maximize the sharing of structure? hash-consing?
    ([] prototypes)
    ((cons x y) (append-prototypes x (append-prototypes y prototypes)))
    ((Poo ps _) (append ps prototypes))
    (_ (error "invalid poo spec" x))))

(def (.mix . poos)
  (Poo (append-prototypes poos) #f))

(def poo? Poo?)

(def (.has? poo slot)
  (match poo
    ((Poo prototypes instance)
     (or (and instance (hash-key? instance slot)) ;; fast check for already-present slot, also includes slots from .set!
         (let/cc return
           (for-each! prototypes (λ (p) (when (hash-key? p slot) (return #t))))
           #f)))
    (else (error ".has?: not poo" poo slot))))

(def (.all-slots poo)
  (match poo
    ((Poo prototypes _)
     (nest
       (with-list-builder (c))
       (let (h (hash)))
       (for-each! prototypes) (λ (p))
       ((cut hash-for-each <> p)) (λ (k _))
       (unless (hash-key? h k)
         (hash-put! h k #t)
         (c k))))
    (else (error ".all-slots: not poo" poo))))


;; A poo specification is of the form:
;;  (poo ([self]) (super-poo ...) (extra-slot-names-to-bind ...) slot-definitions ...))
(defrules poo ()
  ((_ args ...) (poo/self poo/slots args ...)))

(defrules poo/self ()
  ((_ k () args ...) (k self args ...))
  ((_ k (self) args ...) (k self args ...)))

(defrules poo/slots ()
  ((_ self supers (slots ...) (slot slotspec ...) ...)
   (poo/init self supers (slots ... slot ...) (slot slotspec ...) ...)))

(defrules poo/init ()
  ((_ self (supers ...) slots (slot slotspec ...) ...)
   (Poo (cons (hash (slot (poo/slot-init-form self slots slot slotspec ...)) ...)
              (append-prototypes [supers ...])) #f)))

;; A slot specification has one of these forms:
;; (slot form)
;;    the slot value will be computed by evaluating the form
;; (slot => transformer args ...)
;;    the inherited value is passed to the transformer function
;;    with additional args.
;; (slot (next-method) form)
;;    the slot value will be computed by evaluating the form, wherein
;;    the form (next-method) will evaluate to the inherited value for the slot.
;; (slot)
;;    the slot value will be that of a same-named variable in the current lexical environment.

(defrules poo/slot-init-form (=>)
  ((_ self slots slot form)
   (λ (self super-prototypes)
     (poo/wrap-slots self slots form)))
  ((_ self slots slot => form args ...)
   (λ (self super-prototypes)
     (let ((inherited-value (compute-slot self super-prototypes 'slot)))
       (poo/wrap-slots self slots (form inherited-value args ...)))))
  ((_ self slots slot (next-method) form)
   (λ (self super-prototypes)
     (let ((inherited-value (lazy (compute-slot self super-prototypes 'slot))))
       (let-syntax ((next-method (syntax-rules () ((_) (force inherited-value)))))
         (poo/wrap-slots self slots form)))))
  ((_ self slots slot)
   (λ (self super-prototypes) slot)))

(defrules poo/wrap-slots ()
  ((_ self () form)
   form)
  ((_ self (slot1 slots ...) form)
   (let-syntax ((slot1 (syntax-rules () (_ (.ref self 'slot1)))))
     (poo/wrap-slots self (slots ...) form))))

(defrules defpoo ()
  ((_ name (supers ...) slots slot-defs ...)
   (def name (poo (name) (supers ...) slots slot-defs ...))))

(defrules .get ()
  ((_ poo) poo)
  ((_ poo slot slots ...) (.get (.ref poo 'slot) slots ...)))

(defrules .call ()
  ((_ poo slot args ...) ((.get poo slot) args ...)))

;; TODO: check poo mutability status first
(defrules .def ()
  ((_ poo slot (slots ...) slotspec ...)
   (hash-put! (first (Poo-prototypes poo)) 'slot
              (poo/slot-init-form poo (slot slots ...) slot slotspec ...))))

;; TODO: check mutability status of the outer instance
(defrules .set! ()
  ((_ poo slot value)
   (begin (.instantiate poo)
          (hash-put! (Poo-instance poo) 'slot value))))
