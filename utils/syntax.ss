(export #t)
(import <expander-runtime> :std/sugar)

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

;; Use maybe-intern-symbol instead of string->symbol to avoid DoS attacks
;; that cause you to intern too many symbols and run out of memory.
;; : (Or Symbol String) <- String
(def (maybe-intern-symbol string)
  (or (##find-interned-symbol string) string))

;; Use maybe-intern-symbol instead of string->keyword to avoid DoS attacks
;; that cause you to intern too many keywords and run out of memory.
;; : (Or Keyword String) <- String
(def (maybe-intern-keyword string)
  (or (##find-interned-keyword string) string))

(def (displayify x port)
  (cond
   ((member x '(#f #t () #!void #!eof)) (void))
   ((or (string? x) (symbol? x) (number? x)) (display x port))
   ((keyword? x) (display (keyword->string x) port))
   ((pair? x) (displayify (car x) port) (displayify (cdr x) port))
   ((vector? x) (displayify (vector->list x) port))
   ((AST? x) (displayify (stx-e x) port))
   (else (void))))
(def (stringify . x) (call-with-output-string (lambda (port) (displayify x port))))
(def (symbolify . x) (string->symbol (stringify x)))
(def (keywordify . x) (string->keyword (stringify x)))
(def (maybe-symbolify . x) (maybe-intern-symbol (stringify x)))
(def (maybe-keywordify . x) (maybe-intern-keyword (stringify x)))
