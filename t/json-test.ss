(export json-test)

(import
  :gerbil/gambit
  :std/error :std/srfi/1 :std/sugar :std/text/hex :std/text/json :std/test :std/misc/walist
  :std/text/json/util
  ../base ../json)

(defstruct json-rpc-error
  (code    ;; SInt16
   message ;; String
   data)   ;; (Maybe Bytes)
  transparent: #t)
(defmethod {:json json-rpc-error} trivial-struct->json-object)
(def (json-rpc-error<-json json)
  (trivial-json-object->struct json-rpc-error::t json (hash (data (void)))))

(def json-test
  (test-suite "test suite for clan/json"
    (test-case "trivial-json<-struct, trivial-struct<-json"
      (defrule (t struct alist)
        (begin
          (checkf equal-struct? (json-rpc-error<-json (list->hash-table alist)) struct)
          (check-equal? (string<-json struct) (string<-json (walist alist)))))
      (parameterize ((json-symbolic-keys #t))
        (check equal-struct? (json-rpc-error -1 "foo" [42]) (json-rpc-error -1 "foo" [42]))
        (t (json-rpc-error -1 "foo" [42]) '((code . -1) (message . "foo") (data . (42))))
        (t (json-rpc-error -100 "bar" (void)) '((code . -100) (message . "bar") (data . #!void)))
        (check equal-struct?
               (json-rpc-error<-json (hash (code -2) (message "x")))
               (json-rpc-error -2 "x" (void)))))

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
