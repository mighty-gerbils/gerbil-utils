(export string-test)

(import
  :std/test
  ../string)

(def (dup x) (string-append x x))

(def string-test
  (test-suite "test suite for clan/string"
    (test-case "test string-trim-spaces"
      (check-equal? (string-trim-spaces "banana") "banana")
      (check-equal? (string-trim-spaces "    spaces and tabs   	  ") "spaces and tabs"))
    (test-case "test co-pad-strings"
      (check-equal? (co-pad-strings '("ab" "c" "def" "gh" "ijk")) '("ab " "c  " "def" "gh " "ijk")))
    (test-case "test string-interpolate"
      (check-equal? (string-interpolate "a${b}c$${d}${e}f" dup) "abbc${d}eef"))))
