(export logger-test)

(import
  :std/sugar :std/test
  ../json ../logger)

(def logger-test
  (test-suite "test suite for clan/logger"
    (test-case "test metadata recognition"
      (check-eqv? (metadata-line? (string<-json (metadata))) #t)
      (check-eqv? (metadata-line? (string<-json (hash ("hello" "world")))) #f))))
