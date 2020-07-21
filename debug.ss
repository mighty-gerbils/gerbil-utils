;; -*- Gerbil -*-
;;;; Debugging utilities

(export #t)
(export pr prn repr) ;; reexport from std/misc/repr

(import
  (for-syntax ./syntax :std/misc/repr)
  :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/misc/repr :std/sugar
  ./base ./concurrency)

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

;; NB: fprintf uses the current-error-port and calls force-output
(def (DBG-helper tag dbg-exprs dbg-thunks expr thunk)
  (letrec
      ((f (λ (fmt . args)
            (force-output (current-output-port)) ;; avoid out-of-order issues due to stdout buffering
            (apply fprintf (current-error-port) fmt args)
            (force-output (current-error-port))))
       (v (λ (l)
            (for-each (λ (x) (f " ~r" x)) l)
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

;;; Tracing function -- alternative to the trace and untrace functions from gambit, that
;;; 1. works on bindings, not on function objects
;;; 2. prints values with std/misc/repr rather than Gambit's write
;;; 3. prints the call depth as a number rather than indent ever further right

(def trace-counter (make-parameter 0))

(defrule (trace! f ...) (begin (ignore-errors (trace1 f)) ...))
(defrule (untrace! f ...) (begin (ignore-errors (untrace1 f)) ...))

(defrule (trace1 f more ...)
  (trace-function! 'f f (let ((t (traced-function f 'f more ...)))
                          (fun (f . a) (apply t a))) (λ (v) (set! f v))))
(defrule (untrace1 f) (untrace-function! 'f f (λ (v) (set! f v))))

(def (traced-function f name (port (current-error-port)))
  (λ args (apply-tracing f name port args)))

(def (apply-tracing f name port args)
  (def counter (trace-counter))
  (parameterize ((trace-counter (+ 1 counter)))
    (display-separated
     args port
     prefix: (format ">>> ~d (~a" counter name)
     separate-prefix?: #t
     suffix: ")\n"
     display-element: pr)
    (force-output port)
    (def vs (values->list (apply f args)))
    (display-separated
     args port
     prefix: (format "<<< ~d (~a" counter name)
     separate-prefix?: #t
     suffix: ")\n"
     display-element: pr)
    (display-separated
     vs port
     prefix: "==="
     separate-prefix?: #t
     suffix: "\n"
     display-element: pr)
    (force-output port)
    (apply values vs)))

(def traced-functions (make-hash-table))

(def (trace-function! name f t setter)
  (match (hash-get traced-functions name)
    ([_ tt] (when (eq? f tt) (error "function already traced" name)))
    (#f (void)))
  (hash-put! traced-functions name [f t])
  (setter t)
  (void))

(def (untrace-function! name fun setter)
  (match (hash-get traced-functions name)
    (#f (error "function not being traced" name))
    ([f t]
     (hash-remove! traced-functions name)
     (unless (eq? fun t) (error "traced function was redefined, unregistering it" name))
     (setter f)))
  (void))

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
