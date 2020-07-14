;;;; Support for the gerbil-utils testing convention
;; - Tests are in subdirectories t/ of those containing regular definitions
;; - Test files to load are called foo-test.ss, other files being support files
;; - Each file foo-test.ss contains one main definition foo-test, that is
;;   a test-suite as defined using std/test#test-suite and that is to be run
;;   with std/test#run-test-suite!

(export #t)

(import
  :gerbil/expander
  :std/sort :std/misc/repr :std/sugar :std/test
  ../filesystem ../multicall ../path ../path-config ../ports ../source)

;; Given a directory name (with no trailing /), is it a test directory named "t"?
(def (test-dir? x)
  (equal? "t" (path-strip-directory x)))

;; Given a package directory, find all test directories (named "t") under it.
(def (find-test-directories pkgdir)
  (map shorten-path (find-files pkgdir test-dir? recurse?: (lambda (x) (not (test-dir? x))))))

;; Given a package directory, find all test files (with name ending in "-test.ss")
;; in all test directories (named "t") under it.
(def (find-test-files pkgdir (regex "-test.ss$"))
  (sort (find-regexp-files regex (find-test-directories pkgdir)) string<?))

;; Given a test file, return the name
(def (test-symbol module-name)
  (string->symbol (string-append module-name "#" (path-strip-directory module-name))))

(def (find-file-test test-file pkgdir package-prefix)
  (def module-name
    (string-append package-prefix "/"
                   (path-enough (path-strip-extension (path-simplify test-file)) pkgdir)))
  (import-module (string->symbol (string-append ":" module-name)) #t #t)
  (eval (test-symbol module-name)))

;; TODO: this was in std/make. Define and export it somewhere in std.
(def (read-package-prefix pkgdir)
  (with-catch false
              (lambda () (symbol->string
                     (pgetq package:
                            (call-with-input-file (path-expand "gerbil.pkg" pkgdir)
                              read))))))

;; Create a test name (a string, as mandated by test-case) from a name object.
;; If name is already a string, itself, otherwise, the repr of the object.
(def (make-test-name name)
  (if (string? name) name (repr name)))

;; Given a list of test files under package directory, run each of their tests.
(def (run-tests pkgdir
                regex: (regex "-test.ss$")
                test-files: (test-files (find-test-files pkgdir regex)))
  (def package-prefix (read-package-prefix pkgdir))
  (apply run-tests! (map (cut find-file-test <> pkgdir package-prefix) test-files))
  (test-report-summary!)
  (eqv? 'OK (test-result)))

(def (%set-test-environment! script-path add-load-path)
  (set-current-ports-encoding-standard-unix!)
  (def src (path-normalize (path-directory script-path)))
  (current-directory src)
  (add-load-path src)
  (set! source-directory src)
  (set! home-directory src))

(defsyntax (init-test-environment! stx)
  (syntax-case stx ()
    ((ctx)
     (with-syntax ((main (datum->syntax #'ctx 'main))
                   (add-load-path (datum->syntax #'ctx 'add-load-path)))
     #'(begin
         (%set-test-environment! (this-source-file ctx) add-load-path)
         (def main call-entry-point))))))

(def (all) (run-tests "." test-files: (find-test-files ".")))
(def (integration) (run-tests "." test-files: (find-test-files "." "-integrationtest.ss$")))
(def (test . files) (run-tests "." test-files: files))

(register-entry-point "all" all help: "Run all unit tests")
(register-entry-point "integration" integration help: "Run all integration tests")
(register-entry-point "test" test help: "Run specific tests")

(set! multicall-default all)
