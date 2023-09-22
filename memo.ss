;; -*- Gerbil -*-
;;;; memoization (ported from CL's fare-memoization 1.1.0)

(export #t)

(import
  :std/misc/hash
  ./base ./assert)

;; The basic helper for computing with a memoized function as specified by the info structure
;; being called with given arguments.
;;
;; NB: To unmemoize, (1) pass an explicit table argument to the wrapper, don't use #t for the mutex,
;; (2) with the mutex if any, remove the data from the table.
(def (memoizing
      function ;; original function to wrap
      table: (table #f) ;; hash-table to store elements (default #f designates a fresh hash-table)
      normalization: (normalization #f) ;; function to normalize the list of arguments (default #f designates identity)
      call: (call #f) ;; function to call the function with normalized arguments (default #f designates apply)
      mutex: (mutex #t)) ;; mutex for access to hash-table (default #t indicates use a fresh mutex; #f means user promises single-threaded access so won't need a mutex).
  (unless table (set! table (make-hash-table)))
  (unless call (set! call apply))
  (when (eq? mutex #t) (set! mutex (make-mutex 'memoizing)))
  (let* ((compute
          (λ (arguments)
            (values->list (call function arguments))))
         (cached-or-compute
          (λ (arguments)
            (apply values (hash-ensure-ref table arguments (λ () (compute arguments))))))
         (synced-cached-or-compute
          (if mutex
            (λ (arguments)
              (with-lock mutex (λ () (cached-or-compute arguments))))
            cached-or-compute)))
    (if normalization
      (λ arguments (synced-cached-or-compute (apply normalization arguments)))
      (λ arguments (synced-cached-or-compute arguments)))))

;; Like defun, but creates a memoized function.
;; Also, if the name is a CONS, then the first element is the name, and the rest
;; is a list of keyword arguments, TABLE and NORMALIZATION as per MEMOIZE."
(defrules define-memo-function ()
  ((_ ((name . keys) . formals) . body)
   (def name
     (memoizing (lambda formals . body) . keys)))
  ((_ (name . formals) . body)
   (define-memo-function ((name) . formals) . body)))

;;; This is your generic memoized function.
;;;
;;; If you want to make sure that a given function is only ever called once
;;; with the "same" list of arguments and thus ensure that it always returns
;;; the same value for a "same" list of arguments, it is up to YOU
;;; to normalize the arguments of the function you call such that EQUAL
;;; will properly compare argument lists. You may pass any additional
;;; arguments that you don't want memoized in dynamic variable bindings.
;;;
;;; Note that if you use this on an internal function, the function will thereby escape its scope,
;;; and won't be garbage-collected, and neither will all the context it closes over.
;;; In general, this global memoization table is mostly good only for interactive experiment.
;;; When you identify a function that definitely needs memoization, use memoizing.
(def global-memoization-table (make-hash-table))
(def global-memoization-mutex (make-mutex 'memoized-funcall))
(def memoized-funcall
  (memoizing funcall table: global-memoization-table mutex: global-memoization-mutex))
(def (memoized-apply function . arguments)
  (apply apply memoized-funcall function arguments))
