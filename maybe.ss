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
;; (Maybe A) is notably isomorphic to, but different from the type (OrFalse A) from ./orfalse,
;; but neither is canonically isomorphic to (Option A) from ./option because of the treatment
;; of the exceptional value, that is uniformly quoted by the Option but excluded by Maybe or OrFalse.

(def (Option<-Maybe x)
  (and (not (void? x)) (some x)))

(def (Maybe<-Option x)
  (option-get x (void)))

(def (maybe-ref x) (if (void? x) (error "no value") x))
(def (maybe-get x (default #f)) (if (void? x) default x))
(def (maybe-get/default x (default false)) (if (void? x) (default) x))
(def (map/maybe f x) (if (void? x) x (f x)))
(def (list<-maybe x) (if (void? x) [] [x]))
(def (list-map/maybe f l) ;; : (Maybe (List B)) <- (Fun (Maybe B) <- A) (List A)
  (let loop ((a []) (l l))
    (match l
      ([] (reverse a))
      ([h . t] (let (fh (f h)) (unless (void? fh) (loop (cons fh a) t)))))))
(def (bind/maybe x f) (unless (void? x) (f x)))
(def (for-each/maybe f x) (unless (void? x) (f x)))
