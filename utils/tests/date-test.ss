(export date-test)

(import
  :gerbil/gambit/exceptions
  :std/test
  :utils/base :utils/date :utils/number)

(def date-test
  (test-suite "test suite for utils/date"
    (test-case "Check unix-epoch-offset"
      (check-equal? unix-epoch-offset 0))
    (test-case "Check periodically"
      (let* ((counter 0)
             (target (+ (current-timestamp) one-second))
             (count-for-one-second
              (let/cc break
                (periodically (* 52.5 one-millisecond)
                              (λ () (increment! counter)
                                 (when (> (current-timestamp) target)
                                   (break counter)))))))
        (check-equal? (<= 19 count-for-one-second 21) #t)))))
