;;-*- Gerbil -*-
;;; Brace syntax for POO

(export @method @@method)

(import
  (prefix-in (only-in <MOP> @method) @)
  ../utils/base ./poo)

;; {args ...} -> (@method args ...) -> (.o args ...)
;; except that for macro-scope it's -> (.o/ctx #,stx args ...)
(defsyntax (@method stx)
  (syntax-case stx ()
    ((_ args ...)
     (with-syntax ((ctx stx)) #'(.o/ctx ctx args ...)))))
