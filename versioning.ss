;; -*- Gerbil -*-
;;;; Support for introspectabe software version.
;; The software's build system is responsible for initializing variables
;; software-name (based on the software name) and
;; software-version (based on e.g. git describe --tags).

(export #t)

;; NB: For bootstrapping reasons, version.ss depends on this and many things may depend on version.ss,
;; so we make sure we don't depend on anything in clan.
(import
  :gerbil/gambit/system
  :std/format :std/iter :std/misc/list :std/misc/ports :std/misc/process :std/misc/string :std/pregexp)
(extern namespace: #f gerbil-greeting)

;; Name and version of the topmost software layer, typically your application.
;; NB: the (values ...) wrapper below prevent Gerbil constant inlining optimization. Yuck.
(def software-layers [["Gerbil" (gerbil-version-string)...]
                      ["Gambit" (system-version-string)...]])
(def (software-name) (caar software-layers)) ;; : String
(def (software-version) (cdar software-layers)) ;; : String

;; Register the (so far) topmost software layer.
;; If you build your software in layers, a further specialized application may later override it.
;; : <- String String
(def (register-software name version)
  ;; Update the name and version to just the topmost software layer (application)
  (set! software-layers [[name . version] . software-layers]) ;; (aset software-layers name version)
  ;; Update the Gerbil-Greeting to the latest layer
  (set! gerbil-greeting (format "~a ~a" name version)))

;; : String <-
(def (software-identifier (complete #f))
  (apply string-append
    (with-list-builder (p)
      (def layers (if complete software-layers [(car software-layers)]))
      (def l (length layers))
      (for ((i (in-range l)) (layer layers))
        (cond
         ((zero? i) (void))
         ((= i 1) (p " on "))
         (else (p ", ")))
        (match layer ([name . version] (p name) (p " ") (p version)))))))

;; <- (Optional Port)
(def (show-version complete: (complete #f) port: (port (current-output-port)))
  (fprintf port "~a\n" (software-identifier complete)))

;; TODO: use FFI for that -- except it differs on Linux, BSD (mac?), Windows.
(def machine-name (let (d (delay (##os-host-name))) (cut force d)))
