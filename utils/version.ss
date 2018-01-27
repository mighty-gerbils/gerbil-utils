;; -*- Gerbil -*-
;;;; Support for introspectabe software version.
;; The software's build system is responsible for initializing variables
;; software-name (based on the software name) and
;; software-version (based on e.g. git describe --tags).

(export #t)

(import
  :std/format :std/misc/ports :std/misc/process :std/misc/string :std/pregexp
  :clan/utils/base :clan/utils/basic-parsers)
(extern namespace: #f gerbil-greeting)

;; Name and version of the topmost software layer, typically your application.
;; NB: the (values ...) wrapper below prevent Gerbil constant inlining optimization. Yuck.
(def software-name (values #f)) ;; : String
(def software-version (values #f)) ;; : String

;; Register the (so far) topmost software layer.
;; If you build your software in layers, a further specialized application may later override it.
;; : <- String String
(def (register-software name version)
  ;; Update the name and version to just the topmost software layer (application)
  (set! software-name name)
  (set! software-version version)
  ;; Update the Gerbil-Greeting to include all layers of software loaded.
  (set! gerbil-greeting (format "~a ~a on ~a" name version gerbil-greeting)))

;; : String <-
(def (software-identifier)
  (string-join `(,@(if software-name [software-name] [])
                 ,@(if software-version [software-version] []))
               #\space))

;; <- (Optional Port)
(def (show-version (port (current-output-port)))
  (fprintf port "~a\n" (software-identifier)))

;; Parse a git description as returned by git describe --tags into a list-encoded tuple of:
;; the top tag in the commit, the number of commits since that tag, and the 7-hex-char commit hash
;; if any was provided (if none, then use the tag).
;; : (Tuple String Nat (Or String #f)) <- String
(def (parse-git-description description)
  (match (pregexp-match "^(.*)-([0-9]+)-g([0-9a-f]{7})$" description)
    ([_ tag commits hash]
     [tag (call-with-input-string commits (λ (port) (expect-natural port))) hash])
    (else
     [description 0 #f])))

(defonce (machine-name)
  (string-trim-eol (run-process ["hostname"])))
