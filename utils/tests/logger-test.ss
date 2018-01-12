(export logger-test)

(import
  :std/sugar :std/test
  :clan/utils/json :clan/utils/logger)

(def logger-test
  (test-suite "test suite for clan/utils/logger"
    (test-case "test metadata recognition"
      (check-eqv? (metadata-line? (json<- (metadata))) #t)
      (check-eqv? (metadata-line? (json<- (hash ("hello" "world")))) #f))))
