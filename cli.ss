(export #t)

(import
  :std/getopt :std/srfi/13 :std/sugar
  ./exit ./json ./with-id)

;; Given a string argument designating a JSON object
;; Json <- String
(def (json<-cli-input cli-input)
  (cond
   ((equal? cli-input "-")
    (json<-port (current-input-port)))
   ((and (string? cli-input)
         (< 0 (string-length cli-input))
         (string-index "[{\"0123456789-" (string-ref cli-input 0)))
    (json<-string cli-input))
   ((string? cli-input)
    (read-file-json cli-input))
   (else (error "invalid input specifier" 'json<-cli-input cli-input))))

(defrule (lambda-opt body ...)
  (lambda (opt)
    (with-id/expr lambda-opt ((@method))
      (defrule (@method x) (hash-get opt 'x)) ;; make is so {x} accesses the option.
      body ...)))

(defstruct option-spec (getopt-spec processor) transparent: #t)

(def option/backtrace
  (make-option-spec
   [(flag 'backtrace "--backtrace"
          help: "enable backtraces for debugging purposes")]
   (lambda-opt (backtrace-on-abort? {backtrace}))))
