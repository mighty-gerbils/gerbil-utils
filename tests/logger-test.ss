(export logger-test)

(import
  :std/sugar :std/test
  :utils/json :utils/logger)

(def logger-test
  (test-suite "test suite for utils/logger"
    (test-case "test metadata recognition"
      (check-eqv? (metadata-line? (json<- (metadata))) #t)
      (check-eqv? (metadata-line? (json<- (hash ("hello" "world")))) #f))))
