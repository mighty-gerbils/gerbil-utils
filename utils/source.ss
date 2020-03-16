(export #t)

;;; Locations follow the Gambit convention: it's a vector of two values.
;;; The first value is either a string which is filename, or a list containing a symbol.
;;; The second value is a fixnum, either non-negative (+ (* 65536 column) line),
;;; or if the previous formula had overflows, negative file position.
(defsyntax (this-source-location stx)
  (syntax-case stx ()
    ((_ ctx) (datum->syntax #'ctx ['quote (stx-source #'ctx)]))
    ((foo) #'(this-source-location foo))))
