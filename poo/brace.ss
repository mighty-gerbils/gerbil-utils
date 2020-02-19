;;-*- Gerbil -*-
;;; Brace syntax for POO

(export @method @@method)

(import
  (prefix-in (only-in <MOP> @method) @)
  :clan/utils/base :clan/poo/poo)

(defrule {args ...} (.o args ...))
