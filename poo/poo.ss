;;-*- Gerbil -*-
;; Trivial implementation of Prototype Object Orientation in Gerbil Scheme.
;;
;; See poo.md for documentation
;; TODO: see Future Features and the Internals TODO sections in document above.

(export
  poo defpoo .mix .ref .instantiate .get .call .set! .def)

(import
  :std/format :std/lazy :std/misc/list :std/misc/rbtree :std/misc/repr
  :std/srfi/1
  :std/sugar
  :clan/utils/base :clan/utils/hash :clan/utils/list)

(defstruct Poo (prototypes layers))

(def (no-such-slot instance slot) (λ () (error "No such slot" instance slot)))

(def (.instantiate poo)
  (match poo
    ((Poo prototypes #f) (set! (Poo-layers poo) (map (λ _ (hash)) prototypes)))
    ((Poo _ _) (void)) ;; already instantiated
    (else (error "No poo" poo))))

(def (.ref poo slot (default (no-such-slot poo slot)))
  (.instantiate poo)
  (match poo
    ((Poo [] []) (default))
    ((Poo prototypes layers)
     (hash-ensure-ref
      (car layers) slot
      (λ () (compute-poo-slot poo prototypes layers slot default))))))

(def (compute-poo-slot poo prototypes layers slot (default (no-such-slot poo slot)))
  (match prototypes
    ([] (default))
    ([prototype . super-prototypes]
     (match layers
       ([layer . super-layers]
        (if-let (fun (hash-get prototype slot))
           (hash-ensure-ref layer slot (λ () (fun poo super-prototypes super-layers)))
           (compute-poo-slot poo super-prototypes super-layers slot default)))))))

(def (append-prototypes x (prototypes []))
  (match x
    ([] prototypes)
    ((cons x y) (append-prototypes x (append-prototypes y prototypes)))
    ((Poo ps _) (append ps prototypes))
    (_ (error "invalid prototype specification" x))))

(def (.mix . poos)
  (Poo (append-prototypes poos) #f))


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

(defrules poo/slot-init-form (=>)
  ((_ self slots slot form)
   (poo/slot-fun self slots slot next-method form))
  ((_ self slots slot => form args ...)
   (poo/slot-fun self slots slot next-method (form (next-method) args ...)))
  ((_ self slots slot (next-method) form)
   (poo/slot-fun self slots slot next-method form))
  ((_ self slots slot)
   (poo/slot-fun self () slot next-method slot)))

(defrules poo/slot-fun ()
  ((_ self slots slot next-method form)
   (λ (self super-prototypes super-layers)
     (let-syntax ((next-method (syntax-rules ()
                                 ((_) (compute-poo-slot self super-prototypes super-layers 'slot)))))
       (poo/wrap-slots self slots form)))))

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
  ((_ poo slot value) (hash-put! (first (Poo-layers poo)) 'slot value)))
