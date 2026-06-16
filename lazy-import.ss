
(export lazy-import)

(import :std/lazy :std/sugar)

(defrule (function-eta expr)
  (lambda args (apply expr args)))

(defrule (lazy-import (modpath (id ...)) ...)
  (begin (lazy-import1 modpath (id ...)) ...))

(begin-syntax
  (def (resolve-modpath p)
    (cond
      ((core-library-module-path? p)
       (core-resolve-library-module-path p))
      ;((core-library-relative-module-path? p)
      ; (core-resolve-library-relative-module-path p))
      ((stx-string? p)
       (core-resolve-module-path p))
      (else
       (error "lazy-import: unsupported modpath" (syntax->datum p))))))

(defsyntax lazy-import1
  (lambda (stx)
    (syntax-case stx ()
      ((_ modpath (id ...))
       (identifier-list? #'(id ...))
       (with-syntax ((p (resolve-modpath #'modpath))
                     ((tmp ...) (gentemps #'(id ...))))
         #'(begin
             (def get-sym (make-get-sym 'p '(id ...) '((id tmp) ...)))
             (def id (make-lazy-function 'tmp get-sym))
             ...))))))

(def (make-get-sym modpath ids ids/tmps)
  (def mod-p (delay (eval `(import (rename-in (only-in ,modpath ,@ids) ,@ids/tmps)))))
  (def (get-sym tmpsym)
    (force mod-p)
    (eval tmpsym))
  get-sym)

(def (make-lazy-function tmpsym get-sym)
  (def fun-p (delay (get-sym tmpsym)))
  (function-eta (force fun-p)))
