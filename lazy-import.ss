
(export lazy-import)

(import :std/lazy :std/sugar)

(defrule (function-eta expr)
  (lambda args (apply expr args)))

(defrule (lazy-import (modpath (id ...)) ...)
  (begin (lazy-import1 modpath (id ...)) ...))

(defsyntax lazy-import1
  (lambda (stx)
    (syntax-case stx ()
      ((_ modpath (id ...))
       (identifier-list? #'(id ...))
       (let ()
         ;(core-resolve-library-module-path (stx-e #'modpath))
         ;; example: "/home/alexknauth/.gerbil/lib/vyzo/libp2p.ssi"
         ;; load that with `load`, and extract:
         ;; path after ".gerbil/lib/", before ".ssi"
         ;; format that with `#` and then the id
         #'(begin
             (def get-sym (make-get-sym 'modpath))
             (def id (make-lazy-function 'id get-sym))
             ...))))))

(def (make-get-sym modpath)
  (def mod-p (delay `(eval (import ,modpath))))
  (def (get-sym sym)
    (force mod-p)
    (eval sym))
  get-sym)

(def (make-lazy-function sym get-sym)
  (def fun-p (delay (get-sym sym)))
  (function-eta (force fun-p)))
