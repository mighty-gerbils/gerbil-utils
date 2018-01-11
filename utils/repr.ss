;; -*- Gerbil -*-
;;;; Sourceable Representation of Gerbil entities

(export #t)

(import
  :gerbil/gambit/hash :gerbil/gambit/ports
  :std/format :std/misc/rtd :std/sort :std/srfi/1
  :utils/base :utils/list)

;; Default options. Keep it empty.
;; Note: we don't actually use options yet, but
;; it is intended to contain pretty-printing state such as desired number of columns, etc.
(def default-representation-options (make-hash-table))

;; TODO: use immutable hash tables instead when available
(def current-representation-options (make-parameter default-representation-options))

(def (display-separated
      list
      port: (port (current-output-port))
      prefix: (prefix "")
      separator: (separator " ")
      suffix: (suffix "")
      display-element: (display-element (λ (x) (display x port))))
  (display prefix port)
  (def first? #t)
  (for-each!
   list
   (λ (elem)
     (if first?
       (set! first? #f)
       (display separator port))
     (display-element elem)))
  (display suffix port))

(def (print-representation
      x
      port: (port (current-output-port))
      options: (options (current-representation-options)))

  ;; Our universal utilities: print (recurse), repr, display, write
  (def (p y) (print-representation y port: port options: options))
  (def (r y) (repr y options: options))
  (def (d y) (display y port))
  (def (w y) (write y port))
  (def (simple? x) (or (number? x) (boolean? x) (string? x) (char? x) (void? x) (eof-object? x)))

  (cond
   ;; Simplest case: just write it.
   ((simple? x)
    (w x))
   ((or (symbol? x) (null? x)) ;; requires slightly more care: write it after a quote.
    (d "'") (w x))
   ((pair? x) ;; pair: print as [ ... ].
    (display-separated x prefix: "[" separator: " " display-element: p port: port)
    (let ((end (cdr (last-pair x))))
      (cond
       ((null? end) (void))
       ((simple? end) (d " . ") (p end))
       (else (d " ") (p end) (d " ..."))))
    (d "]"))
   ((vector? x) ;; vector: print as (vector ...).
    (display-separated (vector->list x)
                       prefix: "(vector " separator: " " display-element: p suffix: ")" port: port))
   ((u8vector? x) ;; u8vector: print as (u8vector ...).
    (display-separated (u8vector->list x)
                       prefix: "(u8vector " separator: " " display-element: p suffix: ")" port: port))
   ((hash-table? x) ;; hash-table: print as (hash ...)
    ;; NB: also assumes (1) it is a equal? table, and (2) you use :std/sugar ...
    (display-separated
     (sort (map (λ (k) (cons (r k) (hash-ref x k))) (hash-keys x)) ;; sort keys by repr
           (λ (krv1 krv2) (string<? (car krv1) (car krv2))))
     port: port prefix: "(hash " suffix: ")"
     display-element: (λ-match ([kr . v] (d "(") (d kr) (d " ") (p v) (d ")")))))
   ((representable? x)
    {:print-representation x port: port options: options})
   ((and (object? x) (find-method (object-type x) ':print-representation))
    => (λ (m) (m x port: port options: options)))
   ((and (object? x) (struct-type? (object-type x)))
    (display-separated
     (cdr (struct->list x))
     port: port prefix: (format "(~a " (##type-name (object-type x))) suffix: ")"
     display-element: p))
   (else
    (print-unrepresentable-object x port: port))))

(def (print-unrepresentable-object
      object
      port: (port (current-output-port))
      options: (options (current-representation-options)))
  (def (d x) (display x port))
  (def (w x) (write x port))
  (d "(begin0 #") (d (object->serial-number object)) (d " ") (w (object->string object)) (d ")"))

(defclass representable ())

;; TODO: have this funtion moved to and exported from :std/misc/rtd
(def (method-for-type type-descriptor name)
  (let ((methods (type-descriptor-methods type-descriptor)))
    (and methods (hash-get methods name))))

(defmethod {:print-representation representable} print-unrepresentable-object)

(def pr (values print-representation))

(def (prn x port: (p (current-output-port)) options: (o (current-representation-options)))
  (pr x port: p options: o) (newline p))

(def (repr x options: (options (current-representation-options)))
  (call-with-output-string
    [] (λ (port) (print-representation x port: port options: options))))
