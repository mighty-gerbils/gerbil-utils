(export assert-test)

(import
  :std/test :std/pregexp
  ../assert)

(def assert-test
  (test-suite "test suite for clan/assert"
    (test-case "test assert!! form"
      (def e 'needle)
      (def l ['stack 'of 'hay])
      (check-exception (assert!! (member e l))
                       (lambda (e)
                         (pregexp-match
                          (string-append
                           "Assertion failed \"t/assert-test.ss\"@12.34: \\(member e l\\)\n"
                           "  e => 'needle\n"
                           "  l => \\['stack 'of 'hay\\]\n")
                          (error-message e)))))))
