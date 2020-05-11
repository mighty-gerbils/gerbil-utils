(export #t)

(import ./option)

;; (Maybe A) is a disjoint union (Union A Unit), with #!void as the unit on the Scheme side,
;; which is represented as null on the JSON side.
;; Should default: be annotations on the type rather than on the record entry? MAYBE!
;; then Maybe would provide default: None, which could be overridden with nodefault: #t,
;; or using (Union A Unit). Now, what does that mean in declaring Scheme functions,
;; where the default is usually #f? Do we have an OrFalse instead of Maybe?
(def None (void))

(def (Option<-Maybe x)
  (and (not (eq? x None)) (some x)))

(def (Maybe<-Option x)
  (option-get x None))

(def (maybe-ref x) (if (eq? x None) (error "no value") x))
(def (maybe-get x (default #f)) (if (eq? x None) default x))
(def (maybe-get/default x (default false)) (if (eq? x None) (default) x))
(def (map/maybe f x) (if (eq? x None) x (f x)))
(def (list<-maybe x) (if (eq? x None) [] [x]))
(def (list-map/maybe f l)
  (let loop ((a []) (l l))
    (match l
      ([] (reverse a))
      ([x . t] (let (fx (f x)) (if (eq? fx None) None (loop (cons fx a) t)))))))
(def (bind/maybe x f) (if (eq? x None) None (f x)))
(def (iter/maybe f x) (bind/maybe x f))
