;;-*- Gerbil -*-
;;; Brace syntax for POO

(export @method @@method)

(import
  (prefix-in (only-in <MOP> @method) @)
  :clan/utils/base :clan/poo/poo)

;; {args ...} -> (@method args ...) -> (.o args ...)
;; except that for macro-scope it's -> (.o/derived #,stx args ...)
(defsyntax @method
  (lambda (stx)
    (syntax-case stx ()
      ((_ args ...)
       (with-syntax ((ctx stx)) #'(.o/derived ctx args ...))))))
