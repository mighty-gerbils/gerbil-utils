(export #t)

(import ./option)

;; (OrFalse A) is a disjoint union (Union A (Exactly #f)),
;; where A is a type that does *not* contain the false element #f.
;;
;; Please use orfalse.ss in case the type A is generic enough that it cannot be guaranteed
;; not to contain the false element.
;;
;; The false element #f is notably different from the Scheme empty list '() often called "null"
;; and recognized by the predicate null?, as well as from the void element (void) aka #!void
;; notably used in the Gerbil standard library to represent JSON null.
;;
;; (OrFalse A) is notably isomorphic to, but different from, the type (Maybe A) from ./maybe,
;; that uses (void) as the exceptional value, but neither is canonically isomorphic
;; to (Option A) from ./option because of the treatment of the exceptional value,
;; that is uniformly quoted by the Option but excluded by Maybe or OrFalse.

(def (Option<-OrFalse x)
  (and (not x) (some x)))

(def (Maybe<-OrFalse x)
  (option-get x #f))

(def (orfalse-ref x) (or x (error "no value")))
(def (orfalse-get x (default #f)) (or x default))
(def (orfalse-get/default x (default false)) (or x (default)))
(def (map/orfalse f x) (and (not x) (f x)))
(def (list<-orfalse x) (if x [x] []))
(def (list-map/orfalse f l) ;; : (OrFalse (List B)) <- (Fun (OrFalse B) <- A) (List A)
  (let loop ((a []) (l l))
    (match l
      ([] (reverse a))
      ([h . t] (let (fh (f h)) (and (not fh) (loop (cons fh a) t)))))))
(def (bind/orfalse x f) (and x (f x)))
(def (for-each/orfalse f x) (when x (f x)))
