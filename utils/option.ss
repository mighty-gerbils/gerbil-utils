(export #t)

;; Options: (some value), or anything else (canonically #f) to mean absent value.
(defstruct some (value) transparent: #t)
(def (option? x) (or (some? x) (not x))) ;; an Option is canonically (some x) or #f
(def (option-ref x) (match x ((some v) v) (else (error "no value" x))))
(def (option-get x (default #f)) (match x ((some v) v) (else default)))
(def (option-get/default x (default false)) (match x ((some v) v) (else (default))))
(def (map/option f m) (match m ((some x) (some (f x))) (else m)))
(def (list<-option x) (match x ((some v) [v]) (else [])))
(def (list-map/option f l)
  (match l
    ([] (some l))
    ([x . t] (match (f x)
               ((some y) (map (cut cons y <>) (list-map/option f t)))
               (e e)))))
(def (bind/option x f) (match x ((some v) (f v)) (else x)))
(def (iter/option f x) (match x ((some v) (some (f v))) (else (void))))
