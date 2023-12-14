;; -*- Gerbil -*-
;;;; Debugging utilities

(export #t (import: :std/debug/DBG))
(export pr prn repr) ;; reexport from std/misc/repr

(import
  (for-syntax ./syntax :std/misc/repr)
  :gerbil/gambit
  :std/format
  :std/debug/DBG
  :std/misc/repr
  :std/sugar
  ./base ./concurrency)

;;; Tracing function -- alternative to the trace and untrace functions from gambit, that
;;; 1. works on bindings, not on function objects
;;; 2. prints values with std/misc/repr rather than Gambit's write
;;; 3. prints the call depth as a number rather than indent ever further right

(def trace-counter (make-parameter 0))

(defrule (trace! f ...) (begin (ignore-errors (trace1 f)) ...))
(defrule (untrace! f ...) (begin (ignore-errors (untrace1 f)) ...))

(defrule (trace1 f more ...)
  (trace-function! 'f f (let ((t (traced-function 'f f more ...)))
                          (fun (f . a) (apply t a))) (λ (v) (set! f v))))
(defrule (untrace1 f) (untrace-function! 'f f (λ (v) (set! f v))))

(def (traced-function name f (port (current-error-port)))
  (λ args (apply-tracing name f port args)))

(def (apply-tracing name f port args)
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
