(export #t)

(import ./option)

;; (Maybe A) is a disjoint union (Union A Null),
;; where the "Null" is a one-element type representing JSON null,
;; which in Scheme is the object #!void.
;; NB: This is different from the Scheme empty list '() often called "null" in Scheme context.
;;
;; Should default: be annotations on the type rather than on the record entry? MAYBE!
;; then Maybe would provide default: null, which could be overridden with nodefault: #t,
;; or using (Union A Null). Now, what does that mean in declaring Scheme functions,
;; where the default is usually #f? Do we have an OrFalse instead of Maybe?
(def null (void))

(def (Option<-Maybe x)
  (and (not (eq? x null)) (some x)))

(def (Maybe<-Option x)
  (option-get x null))

(def (maybe-ref x) (if (eq? x null) (error "no value") x))
(def (maybe-get x (default #f)) (if (eq? x null) default x))
(def (maybe-get/default x (default false)) (if (eq? x null) (default) x))
(def (map/maybe f x) (if (eq? x null) x (f x)))
(def (list<-maybe x) (if (eq? x null) [] [x]))
(def (list-map/maybe f l)
  (let loop ((a []) (l l))
    (match l
      ([] (reverse a))
      ([x . t] (let (fx (f x)) (if (eq? fx null) null (loop (cons fx a) t)))))))
(def (bind/maybe x f) (if (eq? x null) null (f x)))
(def (for-each/maybe f x) (bind/maybe x f))
