;;;; Support for the gerbil-utils testing convention
;; - Tests are in subdirectories t/ of those containing regular definitions
;; - Test files to load are called foo-test.ss, other files being support files
;; - Each file foo-test.ss contains one main definition foo-test, that is
;;   a test-suite as defined using std/test#test-suite and that is to be run
;;   with std/test#run-test-suite!

(export #t)

(import
  :gerbil/gambit
  :gerbil/expander
  :std/error
  :std/format
  :std/getopt
  :std/iter
  :std/misc/bytes
  :std/misc/process
  :std/misc/repr
  :std/sort
  :std/stxutil
  :std/sugar
  :std/test
  :std/text/hex
  ./base ./exit ./filesystem ./git-fu ./io ./multicall
  ./path ./path-config ./ports ./source ./versioning)

;; Given a directory name (with no trailing /), is it a test directory named "t"?
(def (test-dir? x)
  (equal? "t" (path-strip-directory x)))

;; Given a directory name (with no trailing /), is it a dependency directory named "dep"?
(def (dep-dir? x)
  (equal? "dep" (path-strip-directory x)))

;; Given a package directory, find all test directories (named "t") under it.
(def (find-test-directories pkgdir)
  (find-files pkgdir test-dir?
              recurse?: (lambda (x) (not (or (test-dir? x) (dep-dir? x))))))

;; Given a package directory, find all test files (with name ending in "-test.ss")
;; in all test directories (named "t") under it.
(def (find-test-files pkgdir (regex "-test.ss$"))
  (sort (find-regexp-files regex (find-test-directories pkgdir)) string<?))

;; Given a test file, return the name
(def (test-symbol module-name)
  (symbolify module-name "#" (path-strip-directory module-name)))

(def (find-file-test test-file pkgdir package-prefix)
  (def module-name
    (stringify package-prefix "/"
               (path-enough (path-strip-extension (path-simplify test-file)) pkgdir)))
  (import-module (symbolify ":" module-name) #t #t)
  (eval (test-symbol module-name)))

;; TODO: this was in std/make. Define and export it somewhere in std.
(def (read-package-prefix pkgdir)
  (with-catch false
              (cut !> pkgdir
                   (cut path-expand "gerbil.pkg" <>)
                   (cut call-with-input-file <> read)
                   (cut pgetq package: <>)
                   symbol->string)))

;; Create a test name (a string, as mandated by test-case) from a name object.
;; If name is already a string, itself, otherwise, the repr of the object.
(def (make-test-name name)
  (if (string? name) name (repr name)))

;; Given a list of test files under package directory, run each of their tests.
(def (run-tests pkgdir
                regex: (regex "-test.ss$")
                test-files: (test-files (find-test-files pkgdir regex)))
  (def package-prefix (read-package-prefix pkgdir))
  (def tests (map (cut find-file-test <> pkgdir package-prefix) test-files))
  (cond
   ((null? tests) (displayln "No tests found"))
   (else (apply run-tests! tests)
         (test-report-summary!)
         (eqv? 'OK (test-result)))))

(def (%set-test-environment! script-path add-load-path)
  ;;(write [script-path: script-path add-load-path: add-load-path])(newline)
  (set-current-ports-encoding-standard-unix!)
  (def src (path-directory (path-maybe-normalize script-path)))
  (current-directory src)
  (add-load-path src)
  (set-path-config-root! (subpath src "run"))
  (set! application-source-directory (lambda () src))
  (set! application-home-directory (lambda () src))
  (set-default-entry-point! 'unit-tests)
  (current-program (path-strip-directory script-path)))

(defrule (%init-test-environment! ctx)
   (begin
     (def here (this-source-file ctx))
     (with-id ctx (add-load-path main)
       (define-multicall-main ctx)
       (%set-test-environment! here add-load-path))))

(defsyntax (init-test-environment! stx)
  (syntax-case stx ()
    ((ctx) #'(%init-test-environment! ctx))
    ((_ ctx) #'(%init-test-environment! ctx))))

(define-entry-point (test . files)
  (help: "Run specific tests"
   getopt: [(rest-arguments 'files help: "Test files to run")])
  (silent-exit (run-tests "." test-files: files)))

(define-entry-point (unit-tests)
  (help: "Run all unit tests"
   getopt: [])
  (display "Running unit-tests for ") (show-version complete: #t)
  (apply test (find-test-files ".")))

(define-entry-point (integration)
  (help: "Run all integration tests"
   getopt: [])
  (display "Running integration tests for ") (show-version complete: #t)
  (apply test (find-test-files "." "-integrationtest.ss$")))

(def (0x<-random-source (rs default-random-source))
  (def (bytes<-6u32 l)
    (call-with-output-u8vector (lambda (port) (for-each (lambda (x) (write-nat-u8vector x 4 port)) l))))
  (!> rs random-source-state-ref vector->list bytes<-6u32 hex-encode))

(def (random-source<-0x! 0x (rs default-random-source))
  (def (6u32<-bytes b) (call-with-input-u8vector
                        b (lambda (port) (for/collect (_ (in-range 6)) (read-nat-u8vector 4 port)))))
  (!> 0x hex-decode 6u32<-bytes list->vector (cut random-source-state-set! rs <>)))

;; Call this function at the beginning of any test involving randomness.
(def (init-test-random-source!)
  (cond ((getenv "GERBIL_TEST_RANDOM_SOURCE" #f) => random-source<-0x!)
        (else (random-source-randomize! default-random-source)))
  (displayln "To reproduce the random pattern in the following tests, "
             "set the random seed as follows:\n"
             "export GERBIL_TEST_RANDOM_SOURCE="
             (0x<-random-source)))

(define-entry-point (check-git-up-to-date)
  (help: "Check that this git checkout is up-to-date with its target branch"
   getopt: [])
  (def up-to-date? (git-up-to-date-with-branch?))
  (printf "Checkout~a up-to-date with branch ~a\n" (if up-to-date? "" " not") (git-origin-branch))
  (silent-exit up-to-date?))

(def (error-with-message? message)
  (lambda (e)
    (and (Error? e) (equal? (Error-message e) message))))
