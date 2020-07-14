;; -*- Gerbil -*-
;;;; Support for introspectabe software version.
;; The software's build system is responsible for initializing variables
;; software-name (based on the software name) and
;; software-version (based on e.g. git describe --tags).

(export #t)

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

;; Parse a git description as returned by git describe --tags into a list-encoded tuple of:
;; the top tag in the commit, the number of commits since that tag, and the 7-hex-char commit hash
;; if any was provided (if none, then use the tag).
;; : (Tuple String Nat (Or String #f)) <- String
(def (parse-git-description description)
  (match (pregexp-match "^(.*)-([0-9]+)-g([0-9a-f]{7})$" description)
    ([_ tag commits hash]
     [tag (string->number commits) hash])
    (else
     [description 0 #f])))

(def (process-output-line command)
  (with-catch false (cut string-trim-eol (run-process command))))

;; Update the version file from git
;; name: name of the project, e.g. "GNU Hello" (mandatory)
;; repo: where is the git repository compared to the current ./build.ss directory?
;;   use #f or "." if same directory, "..", "../..", "../../.." or such if above.
;;   (optional, default: #f).
;; path: which file will contain the version?
;;   (optional, default: "version.ss").
;; NB: You need to have at least one git tag, as created with e.g. git tag v0.0
(def (update-version-from-git
      name: name
      repo: (repo #f)
      path: (path "version.ss")
      deps: (deps '()))
  (let* ((git-version
          (and (file-exists? (path-expand ".git" (or repo ".")))
               (process-output-line '("git" "describe" "--tags" "--always"))))
         (git-date
          (and git-version (process-output-line '("git" "log" "-1" "--pretty=%ad" "--date=short"))))
         (version-text
          (and git-version
               (format "~a\n~s ;; ~a\n"
                       `(import :clan/versioning ,@(map (cut format ":~a/version" <>) deps))
                       `(register-software ,name ,git-version) git-date)))
         (previous-version-text
          (and version-text ;; no need to compute it if no current version to replace it with
               (file-exists? path)
               (read-file-string path))))
    (if (and version-text (not (equal? version-text previous-version-text)))
      (call-with-output-file [path: path create: 'maybe append: #f truncate: #t]
        (cut display version-text <>)))))

;; TODO: use FFI for that -- except it differs on Linux, BSD (mac?), Windows.
(def machine-name (let (d (delay (string-trim-eol (run-process ["hostname"])))) (cut force d)))
