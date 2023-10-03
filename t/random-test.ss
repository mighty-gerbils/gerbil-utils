(export random-test)

(import
  :std/format :std/sugar :std/test
  ../random)

(def random-test
  (test-suite "test suite for clan/random"
    (test-case "test random-nat"
      (check-equal? (<= 0 (random-nat 100) 99) #t))))
