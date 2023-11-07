;; -*- Gerbil -*-
;;; Autocurrying of functions, to enable Categorical Functional Programming in Scheme
;; * Functions defined with fun and defn will be semantically unary (single-argument)
;; * If more than one argument is specified, each argument passed but the last
;;   will create a closure that remembers previous arguments and waits for the next.
;; * When the last argument is provided (or when there is no argument),
;;   the body will be evaluated in a begin form.
;; * Furthermore, each function defined with defn and each argument received by fun or defn
;;   will be autocurrying when called.
;; * Also, a single unparenthesized argument will be treated the same as if were parenthesized,
;;   in line with typical lambda-calculus, and differently from Scheme style where it means putting
;;   all the arguments (in variable number) in a list. There is only always one single argument.
;; * The %app autocurrying function application form immediately applies each argument
;;   to the provided function, and keeps applying in autocurried style to any further argument.
;;   Thus: (f x y z) will be evaluated like (((f x) y) z)
;; * If no argument is provided, no function call happens, the naked variable value is passed:
;;   Thus: (v) = v = the value bound.
;; * We did not engineer let or any such form make their bound variables autocurrying, but
;;   you can use an internal defn within a function definition, lambda or let to achieve that effect.
;;
;; This is very much like calling functions in ML.
;; The current simple macros do not support either optional or keyword arguments,
;; e.g. like OCaml does. You can use regular def's and lambda's and function calls for that purpose.
;; Extending the macros below to support optional and keyword arguments in a way similar to OCaml
;; is left as an exercise to the gentle meta-evaluator.
;;
;; The definitions are simple enough to be read and understood for any required semantic clarification.

#| ;; To fully inhabit that universe:
(defalias %%app %app)
(defalias λ fun) ;; Unicode symbol
(defalias lambda fun) ;; l-a-m-b-d-a
(defalias def defn)

OR, when you import, say (only rebinding unicode lambda and def):
(import (rename-in :clan/autocurry (fun λ) (defn def)))
|#

(export #t)

;; Apply a curried function to a list of argument
(define (%app_ . a)
  (cond ((null? a) identity)
        ((null? (cdr a)) (car a))
        ((null? (cddr a)) ((car a) (cadr a)))
        (else (apply %app ((car a) (cadr a)) (cddr a)))))

;; Macro optimization of the above
(define-syntax %app
  (syntax-rules ()
    ((_) identity)
    ((_ fun) fun)
    ((_ fun arg) (fun arg))
    ((_ fun arg . more) (%app (fun arg) . more))
    (_ %app_)))

;; Autocurrying anonymous function definition
(define-syntax fun
  (syntax-rules ()
    ((_ () . body) (begin . body))
    ((_ (x) . body) (lambda (v) (defn x v) . body))
    ((_ (x . y) . body) (fun (x) (fun y . body)))
    ((_ v . body) (fun (v) . body)))) ;; also accept a single var without paren, same as with paren

;; Autocurrying named (potentially recursive) function definition
(define-syntax defn
  (syntax-rules ()
    ((_ (pat . vars) . body)
     (defn pat (fun vars . body)))
    ((_ v . body)
     (begin (define tmp (begin . body))
            (define-syntax v (syntax-rules () ((_ . a) (%app tmp . a))
                                              (_ tmp)))))))
