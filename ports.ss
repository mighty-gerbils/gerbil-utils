(export #t)

(import
  :gerbil/gambit/ports
  :std/sugar)


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

(def (writeln x (port (current-output-port)))
  (write x port)
  (newline port)
  (force-output port))

(def (call-with-output o f)
  (cond
   ((port? o) (f o))
   ((not o) (call-with-output-string f))
   ((eq? o #t) (f (current-output-port)))
   ((string? o) (call-with-output-file o f))
   ((list? o) (call-with-output-file o f)))) ;; c-w-o-f options

(defrules with-output ()
  ((_ (o x) body ...) (call-with-output x (lambda (o) body ...)))
  ((_ (o) body ...) (call-with-output o (lambda (o) body ...))))

(def (call-with-input i f)
  (cond
   ((port? i) (f i))
   ((eq? i #t) (f (current-input-port)))
   ((string? i) (call-with-input-string i f))
   ((list? i) (call-with-input-file i f))))

(defrules with-input ()
  ((_ (i x) body ...) (call-with-input x (lambda (i) body ...)))
  ((_ (i) body ...) (call-with-input i (lambda (i) body ...))))
