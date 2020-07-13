(export #t)

(import
  :gerbil/gambit/bytes
  :gerbil/gambit/exceptions
  :std/sugar
  :std/error :std/text/hex :std/test :std/srfi/1
  ../number ../vector)

(def vector-test
  (test-suite "test suite for clan/utils/vector"
    (test-case "vector-scan-index"
      (check-equal? (vector-scan-index plus? #(-10 -6 -2 -1 0 3 8 19)) 5)
      (check-equal? (vector-scan-index plus? #(-20 -16 -12 -11 -10 -9 -8 -3)) #f)
      (check-equal? (vector-scan-index plus? #(3 8 19 23 42 57 83)) 0))
    (test-case "vector-scan-index-right"
      (check-equal? (vector-scan-index-right minus? #(-10 -6 -2 -1 0 3 8 19)) 3)
      (check-equal? (vector-scan-index-right minus? #(-20 -16 -12 -11 -10 -9 -8 -3)) 7)
      (check-equal? (vector-scan-index-right minus? #(3 8 19 23 42 57 83)) #f))
    (test-case "vector-least-index"
      (check-equal? (vector-least-index plus? #(-10 -6 -2 -1 0 3 8 19)) 5)
      (check-equal? (vector-least-index plus? #(-20 -16 -12 -11 -10 -9 -8 -3)) 8)
      (check-equal? (vector-least-index plus? #(3 8 19 23 42 57 83)) 0))
    (test-case "vector-most-index"
      (check-equal? (vector-most-index minus? #(-10 -6 -2 -1 0 3 8 19)) 4)
      (check-equal? (vector-most-index minus? #(-20 -16 -12 -11 -10 -9 -8 -3)) 8)
      (check-equal? (vector-most-index minus? #(3 8 19 23 42 57 83)) 0))))
