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

;; define a function that takes a hash-table as parameter,
;; in the body of which ($ x) is a getter for arguments.
(defrule (lambda-$ body ...)
  (lambda (arguments)
    (with-id/expr lambda-$ (($))
      (defrule ($ x) (hash-get arguments 'x))
      body ...)))

(def getopt-spec/backtrace
   [(flag 'backtrace "--backtrace"
          help: "enable backtraces for debugging purposes")])
(def process-opts/backtrace
  [(lambda-$ (backtrace-on-abort? ($ backtrace)))])
