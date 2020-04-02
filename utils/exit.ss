;; -*- Gerbil -*-
;;;; Support for building a single multicall binary that has all the fricfrac functionality.

(export #t)

(import
  :gerbil/gambit/ports
  :std/format :std/misc/repr
  :clan/utils/base)

(defrule (eval-print-exit body ...) (call-print-exit (λ () body ...)))

(def (silent-exit (bool #t))
  (if bool (values) (void)))

(def (call-print-exit fun)
  (with-catch
   (λ (x) (ignore-errors (fprintf (current-error-port) "~s~%" x)) (exit 2))
   (call/values
     fun
     (λ vs
       ;; (values) prints nothing and counts as true
       ;; (void) prints nothing and counts as false for the purpose of call-print-exit
       (unless (equal? vs '(#!void))
         (for-each prn vs))
       (force-output)
       (exit (match vs ([#f] 1) ([#!void] 1) (_ 0)))))))
