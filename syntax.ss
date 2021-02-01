(export #t (for-syntax #t))
(import
  (for-syntax :std/iter :std/srfi/1)
  :gerbil/gambit/bytes
  <expander-runtime> :gerbil/expander :std/sugar :std/text/hex
  ./basic-parsers)

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
   ((bytes? x) (display (if (bytes-ascii-printable? x) (bytes->string x) (hex-encode x)) port))
   ((pair? x) (displayify (car x) port) (displayify (cdr x) port))
   ((vector? x) (displayify (vector->list x) port))
   ((AST? x) (displayify (stx-e x) port))
   (else (void))))
(def (stringify . x) (call-with-output-string (lambda (port) (displayify x port))))
(def symbolify (case-lambda ((x) (if (symbol? x) x (string->symbol (stringify x))))
                       (x (string->symbol (stringify x)))))
(def keywordify (case-lambda ((x) (if (keyword? x) x (string->keyword (stringify x))))
                       (x (string->keyword (stringify x)))))
(def maybe-symbolify (case-lambda ((x) (if (symbol? x) x (maybe-intern-symbol (stringify x))))
                             (x (maybe-intern-symbol (stringify x)))))
(def maybe-keywordify (case-lambda ((x) (if (keyword? x) x (maybe-intern-keyword (stringify x))))
                              (x (maybe-intern-keyword (stringify x)))))
(def (identifierify stx . x) (datum->syntax stx (apply symbolify x)))

(begin-syntax
;; Return (1) the identifier, (2) what the keyword argument is if any,
;; (3) whether it is optional, (4) what the default value is if it is optional,
;; and (5) whether it is rest argument.
(def (parse-formal f)
  (syntax-case f ()
    (_ (identifier? f) [f #f #f #f #f])
    ((v d) (identifier? #'v) [#'v #f #t #'d #f])
    (_ (error "Invalid formal parameter" f))))

(def (parse-formals formals)
  (let loop ((rvars [])
             (formals formals))
    (syntax-case formals ()
      (r (identifier? #'r) (reverse (cons [#'r #f #t '() #t] rvars)))
      (() (reverse rvars))
      ((k v . more) (stx-keyword? #'k)
       (match (parse-formal #'v)
         ([id _ opt? default _] (loop (cons [id #'k opt? default #f] rvars) #'more))))
      ((v . rest) (loop (cons (parse-formal #'v) rvars) #'rest))
      (_ (error "Invalid formals" formals)))))

(def (call<-parsed-formal parsed-formal)
  (match parsed-formal ([id kw opt? default _] (if kw [kw id] [id]))))

(def (call<-parsed-formals prefix parsed-formals)
  (def rest? (and (not (null? parsed-formals)) (fifth (last parsed-formals))))
  (with-syntax ((((call ...) ...) (map call<-parsed-formal parsed-formals))
                ((prefix ...) prefix)
                ((ap ...) (if rest? #'(apply) [])))
    #'(ap ... prefix ... call ... ...)))

(def (call<-formals prefix formals)
  (call<-parsed-formals prefix (parse-formals formals)))

(def (formals<-nat n)
  (for/collect (i (in-range n)) (gensym))))
