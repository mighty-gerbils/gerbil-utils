(export random-test)

(import
  :std/format :std/sugar :std/test
  ../random)

(def random-test
  (test-suite "test suite for clan/random"
    (test-case "test random-uint"
      (check-equal? (<= 0 (random-uint 100) 99) #t))))
