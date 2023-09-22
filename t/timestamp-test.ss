(export timestamp-test)

(import
  :std/misc/number :std/test
  ../base ../timestamp)

(def timestamp-test
  (test-suite "test suite for clan/timestamp"
    (test-case "Check unix-epoch-offset"
      (check-equal? unix-epoch-offset 0))
    (test-case "Check periodically"
      (let* ((counter 0)
             (target (+ (current-tai-timestamp) one-second))
             (count-for-one-second
              (let/cc break
                (periodically (* 52.5 one-millisecond)
                              (λ () (increment! counter)
                                 (when (> (current-tai-timestamp) target)
                                   (break counter)))))))
        ;; We expect about 20, but on a Mac using Nix, we've seen as little as 11.
        (check-predicate count-for-one-second (lambda (x) (<= 10 x 21)))))))
