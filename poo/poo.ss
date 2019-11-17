;;-*- Gerbil -*-
;; Trivial implementation of Prototype Object Orientation in Gerbil Scheme.
;;
;; See poo.md for documentation
;; TODO: see Future Features and the Internals TODO sections in document above.

(export
  .o .def poo? .mix .ref .instantiate .get .call .set! .def! .put! .has? .all-slots)

(import
  :clan/utils/base :clan/utils/hash :std/lazy :std/misc/list :std/srfi/1 :std/sugar)

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
     (or (and instance (hash-key? instance slot)) ;; fast check for already-computed slot, also includes slots from .put! or .set!
         (any (cut hash-key? <> slot) prototypes)))
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

(defrules .o ()
  ((_ (:: self) slot-defs ...)
   (poo/slots self [] () slot-defs ...))
  ((_ (:: self super slots ...) slot-defs ...)
   (poo/slots self super (slots ...) slot-defs ...))
  ((_ () slot-defs ...)
   (poo/slots self [] () slot-defs ...))
  ((_ slot-defs ...)
   (poo/slots self [] () slot-defs ...)))

(defrules poo/slots ()
  ((_ self super (slots ...) (slot slotspec ...) ...)
   (poo/init self super (slots ... slot ...) (slot slotspec ...) ...)))

(defrules poo/init ()
  ((_ self super slots (slot slotspec ...) ...)
   (Poo (cons (hash (slot (poo/slot-init-form self slots slot slotspec ...)) ...)
              (append-prototypes super)) #f)))

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
  ((_ self () form) form)
  ((_ self (slot1 slots ...) form)
   (let-syntax ((slot1 (syntax-rules () (_ (.ref self 'slot1)))))
     (poo/wrap-slots self (slots ...) form))))

(defrules .def ()
  ((_ (name options ...) slot-defs ...)
   (def name (.o (:: options ...) slot-defs ...)))
  ((_ name slot-defs ...)
   (def name (.o () slot-defs ...))))

(defrules .get ()
  ((_ poo) poo)
  ((_ poo slot slots ...) (.get (.ref poo 'slot) slots ...)))

(defrules .call ()
  ((_ poo slot args ...) ((.get poo slot) args ...)))

(defrules .def! () ;; TODO: check prototype mutability status first
  ((_ poo slot (slots ...) slotspec ...)
   (hash-put! (first (Poo-prototypes poo)) 'slot
              (poo/slot-init-form poo (slot slots ...) slot slotspec ...))))

(def (.put! poo slot value) ;; TODO: check instance mutability status first
  (.instantiate poo)
  (hash-put! (Poo-instance poo) slot value))

(defrules .set! () ((_ poo slot value) (.put! poo 'slot value)))
