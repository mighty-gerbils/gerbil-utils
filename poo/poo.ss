;;-*- Gerbil -*-
;; Trivial implementation of Prototypes Of Objects in Gerbil Scheme. See poo.md

(export
  mix-poo poo-ref poo-put! poo .ref .call .set! .def)

;; TODO:
;; - DOCUMENTATION!!!
;; - export only Object, but neither Instance nor Prototype?
;; - add support for checking constraints when a prototype is instantiated?
;; - handle mutability: finalize prototypes before you may instantiate anything that inherits from them?
;; - mutability: make prototype a structure? add fields to Object and use that instead of Prototypes?
;; - represent prototypes as pure persistent maps vs instances still as stateful hash tables?
;; - better instance representation as vector, indexed based on hash-consed prototype shapes?
;; - mutable instances: non-heritable local state plus sealed inherited slots? separate state slot?

;;XXX: For debugging: (import :std/interactive)

(import
  :std/format :std/lazy :std/misc/list :std/misc/rbtree :std/misc/repr
  :std/srfi/1
  :std/sugar
  :clan/utils/base :clan/utils/hash :clan/utils/list)

(defstruct Instance (layers))

(defrules with-instance-layers ()
  ((_ (layers instance default) body ...)
   (let ((layers (Instance-layers instance)))
     (if (null? layers) (default)
         body))))

(def (no-such-field instance field) (error "No such field" instance field))

;; variant that caches field values globally in addition to each layer that computes them
(def (instance-ref instance field (default (no-such-field instance field)))
  (with-instance-layers (layers instance default)
    (hash-ensure-ref (caar layers) field (λ () (compute-instance-layers-field layers field default)))))

;; variant that only caches values at layers that compute them
(def (layers-ref instance field (default (no-such-field instance field)))
  (with-instance-layers (layers instance default)
     (compute-instance-layers-field layers field default)))

(def (compute-instance-layers-field layers field (default (no-such-field instance field)))
  (cond
   ((null? layers) (default))
   ((hash-get (cdar layers) field) =>
    (λ (fun) (hash-ensure-ref (caar layers) field fun)))
   (else (compute-instance-layers-field (cdr layers) field default))))

(def (instance-put! instance field value)
  (with-instance-layers (layers instance (λ () (error "trying to modify the empty instance")))
     (hash-put! (first layers) field value)))

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

;; TODO: Maybe next-field should use syntax like
;; (slot => transformer args ...) to filter the next-field through a transformer function,
;; or (slot (next-field) form) to lazily compute the next field
;; instead of (slot form) to plainly assign a value oblivious of inherited ones.

(defsyntax (wrap-next-field stx)
  (syntax-case stx ()
    ((_ self super slot form)
     (with-syntax ((next-field (syntax-local-introduce 'next-field)))
       #'(let-syntax ((next-field (syntax-rules () ((_) (layers-ref super 'slot))))) form)))))

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

(def (poo-ref obj field (default false))
  (match obj
    ((Poo _) (instance-ref (poo-instance obj) field))
    ((Instance _) (instance-ref obj field))
    (else (error "Not a prototype object" obj field))))

(def (collect-prototypes c obj)
  (match obj
    ((Poo prototypes _) (for-each c prototypes))
    ((? hash?) obj)
    ((? list?) (for-each (cut collect-prototypes c) obj))
    (_ (error "invalid prototype specification" obj))))

(def (prototypes<- obj)
  (with-list-builder (c) (collect-prototypes c poos)))

(def (mix-poo . poos)
  (poo<-proto (prototypes<- poos)))

(defrules poo ()
  ((_ (supers ...) selfsuper slots slot-defs ...)
   (poo<-proto (cons (proto selfsuper slots slot-defs ...) (append-map Poo-prototypes [supers ...])))))

(defrules .ref ()
  ((_ obj) obj)
  ((_ obj field fields ...) (_ (poo-ref obj 'field) fields ...)))

(defrules .ref ()
  ((_ obj) obj)
  ((_ obj field fields ...) (_ (poo-ref obj 'field) fields ...)))

(defrules .call ()
  ((_ obj field args ...) ((.ref obj field) args ...)))

;; TODO: check mutability status of the first prototype
(defrules .def ()
  ((_ obj field (self super) form)
   (hash-put! (first (Poo-prototypes poos)) 'field (λ (self super) form))))

;; TODO: check mutability status of the outer instance
(defrules .set! ()
  ((_ obj field value) (hash-put! (first (Poo-instances obj)) 'field value)))
