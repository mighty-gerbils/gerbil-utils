(export #t)

(import ./option)

;; (Maybe A) is a disjoint union (Union A Unit),
;; where the "Unit" is a type containing a single element, the Scheme object #!void or (void),
;; recognized by the predicate void?, which incidentally represents JSON null in std/text/json.
;; NB: This is different from the Scheme empty list '() often called "null" in Scheme context.
;;
;; Should default: be annotations on the type rather than on the record entry? MAYBE!
;; then Maybe would provide default: (void), which could be overridden with nodefault: #t,
;; or explicitly using (Union A Unit). Now, what does that mean in declaring Scheme functions,
;; where the default is usually #f? Do we have an OrFalse instead of Maybe?
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
