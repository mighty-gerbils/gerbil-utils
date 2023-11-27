(export #t)

(import
  :std/srfi/13
  (only-in :std/cli/getopt flag)
  (only-in ./hash hash-removed)
  (only-in ./json read-file-json json<-port json<-string))

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

(def getopt-spec/backtrace
   [(flag 'backtrace "--backtrace"
          help: "enable backtraces for debugging purposes")])
(def process-opts/backtrace
  [(lambda (opt) '(dump-stack-trace? (hash-removed opt 'backtrace)))]) ;; Only in v0.19
