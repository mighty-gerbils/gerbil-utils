(export json-test)

(import
  :gerbil/gambit/bytes :gerbil/gambit/exceptions
  :std/error :std/srfi/1 :std/sugar :std/text/hex :std/text/json :std/test
  ../base ../json)

(defstruct (json-rpc-error exception)
  (code    ;; SInt16
   message ;; String
   data)   ;; (Maybe Bytes)
  transparent: #t)
(defmethod {:json json-rpc-error} trivial-json<-struct)
(def (json-rpc-error<-json json)
  (trivial-struct<-json json-rpc-error::t json (hash (data (void)))))

(def json-test
  (test-suite "test suite for clan/json"
    (test-case "trivial-json<-struct, trivial-struct<-json"
      (defrule (t struct json)
        (begin
          (checkf equal-struct? (json-rpc-error<-json json) struct)
          (check-equal? {:json struct} json)))
      (parameterize ((json-symbolic-keys #t))
        (check equal-struct? (json-rpc-error -1 "foo" [42]) (json-rpc-error -1 "foo" [42]))
        (t (json-rpc-error -1 "foo" [42]) (hash (code -1) (message "foo") (data [42])))
        (t (json-rpc-error -100 "bar" (void)) (hash (code -100) (message "bar") (data (void))))
        (check equal-struct?
               (json-rpc-error<-json (hash (code -2) (message "x")))
               (json-rpc-error -2 "x" (void)))))))
