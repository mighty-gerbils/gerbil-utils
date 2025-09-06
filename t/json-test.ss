(export json-test)

(import
  :gerbil/gambit
  :std/error :std/srfi/1 :std/sugar :std/text/hex :std/text/json :std/test :std/misc/walist
  :std/text/json/util
  ../base ../json)

(def json-test
  (test-suite "test suite for clan/json"
    (test-case "json-object-ref, json-object-get"
      (check-equal? (json-object-ref (hash ("a" "apple") ("b" "banana")) "a") "apple")
      (check-equal? (json-object-ref (hash ("a" "apple") ("b" "banana")) 'a) "apple")
      (check-equal? (json-object-ref (hash ("a" "apple") ("b" "banana")) "b") "banana")
      (check-equal? (json-object-ref (hash ("a" "apple") ("b" "banana")) 'b) "banana")
      (check-equal? (json-object-ref (hash (a "apple") (b "banana")) "a") "apple")
      (check-equal? (json-object-ref (hash (a "apple") (b "banana")) 'a) "apple")
      (check-equal? (json-object-ref (hash (a "apple") (b "banana")) "b") "banana")
      (check-equal? (json-object-ref (hash (a "apple") (b "banana")) 'b) "banana")
      (check-equal? (json-object-ref (walist '(("a" . "apple") ("b" . "banana"))) "a") "apple")
      (check-equal? (json-object-ref (walist '(("a" . "apple") ("b" . "banana"))) 'a) "apple")
      (check-equal? (json-object-ref (walist '(("a" . "apple") ("b" . "banana"))) "b") "banana")
      (check-equal? (json-object-ref (walist '(("a" . "apple") ("b" . "banana"))) 'b) "banana")
      (check-equal? (json-object-get (walist '(("a" . "apple") ("b" . "banana"))) 'b) "banana")
      (check-equal? (json-object-get (walist '(("a" . "apple") ("b" . "banana"))) "c") #f))))
