;;-*- Gerbil -*-
(export #t)

;; Trivial implementation of Prototypes Of Objects in Gerbil Scheme
;;
;; An Instance is a list of layers of fields, each layer being a cons of
;; (a) a table mapping field to value together with
;; (b) a table mapping field to function to lazily compute the value.
;; type Instance = Instance (List (Cons (Map Any <-- Symbol) (Fn Any <-- Symbol)))
;;
;; A Prototype is a table mapping a field to a function to compute that field from
;; (a) an instance for the self fields (including layers for all prototypes), and
;; (b) and instance for the super fields (only layers corresponding to parent prototypes).
;; type Prototype = (Map (Fn Any <-- Instance Instance) <-- Symbol)
;;
;; You can compute an instance as the fixed point of computing fields from a list of prototypes;
;; each prototype may define fields that will be computed from the complete "self" instance,
;; and the "super" instance with layers for the parent prototypes that it inherits from.
;;
;; Some Poo is the datum of a list of prototypes and the lazily computed instance for those prototypes.
;;
;; A macro (poo ...) is provided to make defining objects easier. To be improved.
;;
;; TODO: add support for checking constraints when a prototype is instantiated.

;;XXX: For debugging: (import :std/interactive)

(import
  :std/format :std/lazy :std/misc/list :std/misc/rbtree :std/misc/repr
  :std/srfi/1
  :std/sugar
  :clan/utils/base :clan/utils/list)


;; TODO: use pure persistent maps instead of stateful hash tables for prototype representation,
;; but still hash for instance representation?
;; Even better instance representation:
;; vector, with hash-consed prototype table shapes for name to index map.

;; Below: instances and prototypes in 21 lines of code!
(defstruct Instance (layers))

(def (check-layers layers field)
  (when (null? layers) (error "Field undefined" field)))

;; variant that caches field values globally in addition to each layer that computes them
(def (instance-ref instance field)
  (def layers (Instance-layers instance))
  (check-layers layers field)
  (hash-ensure-ref
   (caar layers) field (λ () (compute-instance-layers-field layers field))))

;; variant that only caches values at layers that compute them
(def (instance-ref% instance field)
  (def layers (Instance-layers instance))
  (check-layers layers field)
  (compute-instance-layers-field layers field))

(def (compute-instance-layers-field layers field)
  (cond
   ((null? layers) (error "Field undefined" field))
   ((hash-get (cdar layers) field) =>
    (λ (fun) (hash-ensure-ref (caar layers) field fun)))
   (else (compute-instance-layers-field (cdr layers) field))))

(def (instantiate-prototypes prototypes)
  (def instance (Instance []))
  (def (layer proto super-layers)
    (def fields (hash))
    (def super (Instance super-layers))
    (def compute (hash))
    (hash-for-each (λ (field fun) (hash-put! compute field (λ () (fun instance super)))) proto)
    (cons (cons fields compute) super-layers))
  (set! (Instance-layers instance) (foldr layer [] prototypes))
  instance)

(defrules proto** ()
  ((_ (self super) (slot form) ...)
   (hash (slot (λ (self super) form)) ...)))

(defsyntax (wrap-next-field stx)
  (syntax-case stx ()
    ((_ self super slot form)
     (with-syntax ((next-field (syntax-local-introduce 'next-field)))
       #'(let-syntax ((next-field (syntax-rules () ((_) (instance-ref% super 'slot))))) form)))))

(defrules wrap-slots (next-field)
  ((_ self super slot () form)
   (wrap-next-field self super slot form))
  ((_ self super slot (slot1 slots ...) form)
   (let-syntax ((slot1 (syntax-rules () (_ (instance-ref self 'slot1)))))
     (wrap-slots self super slot (slots ...) form))))

(defrules proto* ()
  ((_ (self super) slots (slot form) ...)
   (proto** (self super) (slot (wrap-slots self super slot slots form)) ...))
  ((_ () slots (slot form) ...)
   (proto* (self super) slots (slot form) ...)))

(defrules proto ()
  ((_ selfsuper (slots ...) (slot form) ...)
   (proto* selfsuper (slots ... slot ...) (slot form) ...)))

(defstruct Poo (prototypes instance))

(def (poo<-proto prototypes)
  (Poo prototypes #f))

(def (poo-instance obj)
  (unless (Poo-instance obj)
    (set! (Poo-instance obj) (instantiate-prototypes (Poo-prototypes obj))))
  (Poo-instance obj))

(def (poo-ref obj field)
  (match obj
    ((Poo _) (instance-ref (poo-instance obj) field))
    ((Instance _) (instance-ref obj field))))

(def (mix-poo . poos)
  (poo<-proto (append-map Poo-prototypes poos)))

(defrules poo ()
  ((_ (supers ...) selfsuper slots slot-defs ...)
   (poo<-proto (cons (proto selfsuper slots slot-defs ...) (append-map Poo-prototypes [supers ...])))))
