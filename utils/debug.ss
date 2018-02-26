;; -*- Gerbil -*-
;;;; Debugging utilities

(export #t)
(export pr prn repr) ;; reexport from std/misc/repr

(import
  :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/misc/repr
  :clan/utils/base :clan/utils/concurrency)

;; DBG macro for easier print-debugging
;; as ported from Common Lisp's ASDF (in asdf/uiop/contrib/debug.lisp).
;;
;; Usage: (DBG tag forms ...)
;;
;; tag is typically a constant string or keyword to identify who is printing,
;; but can be an arbitrary expression returning a tag to be display'ed first;
;; if the expression returns #f, nothing is printed.
;;
;; forms are expressions, which when the tag was not #f are evaluated in order,
;; with their source code then their return values being write'n each time.
;; The last expression is *always* evaluated and its multiple values are returned,
;; but its source and return values are only printed if tag was not #f;
;; previous expressions are not evaluated at all if tag was #f.
;; The macro expansion has relatively low overhead in space or time.
;;
(defrules DBG ()
  ((_ tag-expr)
   (DBG-helper tag-expr '() '() #f #f))
  ((_ tag-expr dbg-expr ... expr)
   (let ((tagval tag-expr)
         (thunk (λ () expr)))
     (if tagval
       (DBG-helper tagval '(dbg-expr ...) (list (λ () dbg-expr) ...)
                   'expr thunk)
       (thunk)))))

(def DBG-port (values ##stderr-port))

;; NB: fprintf uses the current-error-port and calls force-output
(def (DBG-helper tag dbg-exprs dbg-thunks expr thunk)
  (letrec
      ((f (λ (fmt . args)
            (apply fprintf DBG-port fmt args)
            (force-output DBG-port)))
       (v (λ (l)
            (for-each (λ (x) (f " ~a" (repr x))) l)
            (f "~%")))
       (x (λ (expr thunk)
            (f "  ~s =>" expr)
            (call-with-values thunk (λ x (v x) (apply values x))))))
    (if tag
        (begin
          (f "~a~%" tag)
          (for-each x dbg-exprs dbg-thunks)
          (if thunk (x expr thunk) (void)))
        (if thunk (thunk) (void)))))


;;; Tracing threads

(def traced-threads (make-hash-table))
(def (thread-send-trace-hook dst msg)
  (let ((src (current-thread)))
    (when (or (hash-get traced-threads src) (hash-get traced-threads dst))
      (DBG "MSG!" src dst msg))
    msg))
(def (thread-send-trace . args)
  (for-each (λ (x) (hash-put! traced-threads x #t)) args)
  (set! ##thread-send-hook thread-send-trace-hook))
(def (thread-send-untrace . args)
  (if (null? args)
    (set! traced-threads (make-hash-table))
    (for-each (λ (x) (hash-remove! traced-threads x)) args)))
(def (reset-thread-send-hook)
  (set! ##thread-send-hook #f))
