(export #t)
(import :gerbil/expander :std/sugar)

;;; Allowing for keywords in macros
(def (stx-separate-keyword-arguments args (positionals-only? #f)) ;; see std/misc/list#separate-keyword-arguments
  (let lp ((rest (syntax->list args)) (positionals []) (keywords []))
    (match rest
      ([(? (cut stx-eq? #!rest <>)) . r] (values (foldl cons (if positionals-only? r (cons #!rest r)) positionals) (reverse keywords)))
      ([(? (cut stx-eq? #!key <>)) k . r] (lp r (if positionals-only? (cons k positionals) (cons* k '#!key positionals)) keywords))
      ([(? stx-keyword? k) v . r] (lp r positionals (cons* v (stx-e k) keywords)))
      ([a . r] (lp r (cons a positionals) keywords))
      ([] (values (reverse positionals) (reverse keywords))))))
(def (stx-apply fun args)
  (defvalues (positionals keywords) (stx-separate-keyword-arguments args))
  (apply fun (append keywords positionals)))
(def (stx-call fun . args) (stx-apply fun args))
(defrule (stx-lambda formals body ...) (lambda args (stx-apply (lambda formals body ...) args)))
(defrule (def-stx (name . formals) body ...) (def name (stx-lambda formals body ...)))
(defrule (defsyntax-stx (name . formals) body ...)
  (defsyntax (name stx) (stx-apply (lambda formals body ...) (cdr (syntax->list stx)))))
(defrule (defsyntax-stx/form (name . formals) body ...)
  (defsyntax (name stx) (stx-apply (lambda formals body ...) (cons stx (cdr (syntax->list stx))))))
