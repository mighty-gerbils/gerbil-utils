;; -*- Gerbil -*-
;;;; List utilities
;; TODO: upstream utilities that can be upstreamed to std/misc/list. Keep the rest here.

(export #t)

(import
  :std/misc/list :std/srfi/1
  :clan/utils/base)

(defrules with-list-builder ()
  ((_ (c r) body1 body+ ...) (call-with-list-builder (λ (c r) body1 body+ ...)))
  ((_ (c) body1 body+ ...) (with-list-builder (c _) body1 body+ ...)))

;; Variant of for-each with arguments reversed, which nest-s nicer.
;; The name also makes it more obvious that this is used for side-effects.
;; Unlike for-each, also works on improper lists, ended by non-pairs other than '()
;; : <- (list X) (<- X)
(def (for-each! list fun)
  (match list
    ([elem . more] (fun elem) (for-each! more fun))
    (_ (void))))

;; Variant of map with arguments reversed, which nest-s nicer.
;; : (list Y) <- (list X) (Y <- X)
(def (list-map list fun)
  (map fun list))

(def (group-by n list)
  (cond
   ((null? list) [])
   ((length<=n? list n) [list])
   (else (let-values (((head tail) (split-at list n))) (cons head (group-by n tail))))))


;; Given a predicate, a list and a value to return in the special case that the list is empty,
;; return the special case if the list is empty, otherwise, the smallest element in the list,
;; where the predicate returns true when its first argument is smaller than its second argument.
;; : X <- (Bool <- X X) (list X) X
(def (extremum<-list pred lst (empty-case #f))
  (match lst
    ([] empty-case)
    ([hd . tl]
     (foldl (λ (x y) (if (pred x y) x y)) hd tl))))

;; Analog to CL:PUSH, hence the argument order.
;; TODO: use setq-macro, look at the set! defn in prelude/core.ss
(defrules push! ()
  ((_ element list) (set! list (cons element list))))

;; Given an element of a monoid and a fold function for the monoid,
;; extract a list of the elements in the monoid.
;; (List A) <- (Monoid A) (B <- (Monoid A) B (B <- A B))
(def (list<-monoid m fold) (fold m '() cons))
