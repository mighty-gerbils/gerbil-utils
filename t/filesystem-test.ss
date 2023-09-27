(export filesystem-test)

(import
  :std/test
  (only-in :std/source this-source-file this-source-path)
  ../filesystem)

(def filesystem-test
  (test-suite "test suite for clan/filesystem"
    (test-case "test path-is-script?"
      (check-equal? (path-is-script? (this-source-file)) #f)
      (check-equal? (path-is-script? (this-source-path "does-not-exist.nope")) #f)
      (check-equal? (path-is-script? (this-source-path "../build.ss")) #t))))
