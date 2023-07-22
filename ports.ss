(export #t
  ;; For backward-compatibility, reexport symbols that were adopted upstream
  output-contents force-current-outputs writeln call-with-output with-output call-with-input with-input)

(import
  :gerbil/gambit/ports
  (only-in :std/misc/ports ;; import the symbols to reexport
           output-contents force-current-outputs writeln
           call-with-output with-output call-with-input with-input))

;; NB: the char-encoding: works in open-file but the eol-encoding: doesn't. Sigh.
(def standard-unix-encoding '(char-encoding: UTF-8 eol-encoding: lf))

(def (set-port-encoding-standard-unix! port)
  (port-settings-set! port standard-unix-encoding))

(def (set-current-ports-encoding-standard-unix!)
  (set-port-encoding-standard-unix! (current-input-port))
  (set-port-encoding-standard-unix! (current-output-port))
  (set-port-encoding-standard-unix! (current-error-port)))
