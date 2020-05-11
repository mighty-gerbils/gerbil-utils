(export temporary-files-test)

(import
  :std/test
  :clan/utils/base :clan/utils/temporary-files)

(def temporary-files-test
  (test-suite "test suite for clan/utils/temporary-files"
    (test-case "test use after initialization"
      (check-equal?
       (call-with-temporary-file
        while-open: (λ (port path) (display "hello, world!" port))
        after-close: (λ (path) (call-with-input-file path (λ (port) (read-line port)))))
       "hello, world!"))
    (test-case "test deletion"
      (let ((tmp (call-with-temporary-file after-close: (λ (path) path))))
        (check-eqv? (file-exists? tmp) #f)))
    (test-case "test keep"
      (let ((tmp (call-with-temporary-file after-close: (λ (path) path) keep: #t)))
        (check-eqv? (file-exists? tmp) #t)
        (delete-file-if-exists tmp)))))
