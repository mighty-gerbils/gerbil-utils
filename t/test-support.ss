;;;; Support for the gerbil-utils testing convention
;; - Tests are in subdirectories t/ of those containing regular definitions
;; - Test files to load are called foo-test.ss, other files being support files
;; - Each file foo-test.ss contains one main definition foo-test, that is
;;   a test-suite as defined using std/test#test-suite and that is to be run
;;   with std/test#run-test-suite!

(export #t)

(import
  :gerbil/expander
  :std/format :std/sort :std/misc/process :std/misc/repr :std/sugar :std/test
  ../exit ../filesystem ../multicall ../path ../path-config ../ports ../source)

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
  (set! source-directory (lambda () src))
  (set! home-directory (lambda () src)))

(defsyntax (init-test-environment! stx)
  (syntax-case stx ()
    ((ctx)
     (with-syntax ((main (datum->syntax #'ctx 'main))
                   (add-load-path (datum->syntax #'ctx 'add-load-path)))
     #'(begin
         (%set-test-environment! (this-source-file ctx) add-load-path)
         (def main call-entry-point))))))

(define-entry-point (test . files)
  "Run specific tests"
  (silent-exit (run-tests "." test-files: files)))

(define-entry-point (all)
  "Run all unit tests"
  (test (find-test-files ".")))

(define-entry-point (integration)
  "Run all integration tests"
  (test (find-test-files "." "-integrationtest.ss$")))

(set-default-entry-point! "all")

;; TODO: support doing it in another directory?
(def (gerbil.pkg)
  (with-catch false (lambda () (call-with-input-file "gerbil.pkg" read))))

(def (git-origin-repo)
  (or (pgetq repo: (gerbil.pkg)) "origin"))

(def (git-origin-branch)
  (or (pgetq branch: (gerbil.pkg)) "master"))

(def (git-merge-base . commitishs)
  (run-process ["git" "merge-base" . commitishs] coprocess: read-line))

(define-entry-point (check-git-up-to-date)
  "Check that this git checkout is up-to-date with its target branch"
  (def branch (git-origin-branch))
  (run-process ["git" "fetch" "--depth" "1" (git-origin-repo) branch])
  (def up-to-date? (equal? (git-merge-base "FETCH_HEAD" "FETCH_HEAD")
                           (with-catch false (cut git-merge-base "HEAD" "FETCH_HEAD"))))
  (printf "Checkout~a up-to-date with branch ~a\n" (if up-to-date? "" " not") branch)
  (silent-exit up-to-date?))
