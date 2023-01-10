(export #t)

(import ./option)

;; (Maybe A) is a disjoint union (Union A Unit),
;; where the "Unit" is a type containing a single element, the Scheme object #!void or (void),
;; recognized by the predicate void?, which incidentally represents JSON null in std/text/json,
;; and where A is a type that does *not* contain the unit element.
;;
;; Please use option.ss in case the type A is generic enough that it cannot be guaranteed
;; not to contain the unit element
;;
;; The unit element is notably different from the Scheme empty list '() often called "null"
;; and recognized by the predicate null?, as well as from the boolean false value #f.
;;
;; (Maybe A) is notably isomorphic to but different from the notional type (OrFalse A)
;; disjoint union of A and the singleton type containing the boolean false, a.k.a. (Union A '#f).

(def (Option<-Maybe x)
  (and (not (void? x)) (some x)))

(def (Maybe<-Option x)
  (option-get x (void)))

(def (maybe-ref x) (if (void? x) (error "no value") x))
(def (maybe-get x (default #f)) (if (void? x) default x))
(def (maybe-get/default x (default false)) (if (void? x) (default) x))
(def (map/maybe f x) (if (void? x) x (f x)))
(def (list<-maybe x) (if (void? x) [] [x]))
(def (list-map/maybe f l)
  (let loop ((a []) (l l))
    (match l
      ([] (reverse a))
      ([x . t] (let (fx (f x)) (if (void? fx) (void) (loop (cons fx a) t)))))))
(def (bind/maybe x f) (if (void? x) x (f x)))
(def (for-each/maybe f x) (bind/maybe x f))
