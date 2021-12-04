(export #t)

;;; Locations follow the Gambit convention: it's a vector of two values.
;;; The first value is either a string which is filename, or a list containing a symbol.
;;; The second value is a fixnum, either non-negative (+ (* 65536 column) line),
;;; or if the previous formula had overflows, negative file position.
(defsyntax (this-source-location stx)
  (syntax-case stx ()
    ((_ ctx) (datum->syntax #'ctx ['quote (stx-source #'ctx)]))
    ((foo) #'(this-source-location foo))))

(defsyntax (this-source-file stx)
  (syntax-case stx ()
    ((_ ctx) (datum->syntax #'ctx ['quote (alet (loc (stx-source #'ctx)) (vector-ref loc 0))]))
    ((foo) #'(this-source-file foo))))

(defsyntax (this-source-directory stx)
  (syntax-case stx ()
    ((_ ctx)  #'(path-directory (this-source-file ctx)))
    ((foo) #'(this-source-directory foo))))

(defsyntax (this-source-position stx)
  (syntax-case stx ()
    ((_ ctx) (datum->syntax #'ctx ['quote (alet (loc (stx-source #'ctx)) (vector-ref loc 1))]))
    ((foo) #'(this-source-position foo))))
