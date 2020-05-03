(export #t)

(import :gerbil/gambit/ports)


;; Output some contents to a port.
;; The contents can be a string (display'ed), a u8vector (written),
;; or a procedure (called with the port as argument)
(def (output-contents contents port)
  (cond
   ((string? contents) (display contents port))
   ((u8vector? contents) (write-u8vector contents port)) ;; TODO: does this retry on incomplete write?
   ((procedure? contents) (contents port))
   (else (error "invalid contents" contents))))

(def standard-unix-encoding '(char-encoding: UTF-8 eol-encoding: lf))

(def (set-port-encoding-standard-unix! port)
  (port-settings-set! port standard-unix-encoding))

(def (set-current-ports-encoding-standard-unix!)
  (set-port-encoding-standard-unix! (current-input-port))
  (set-port-encoding-standard-unix! (current-output-port))
  (set-port-encoding-standard-unix! (current-error-port)))

(def (force-current-outputs)
  (force-output (current-output-port))
  (force-output (current-error-port)))
