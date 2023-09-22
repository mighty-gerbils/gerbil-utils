(export json-rpc-test)

(import
  :std/misc/repr :std/sugar :std/test :std/text/json
  :clan/base :clan/json :clan/maybe
  :std/net/json-rpc)

(def json-rpc-test
  (test-suite "test suite for clan/net/json-rpc"
    '(test-case "encode params"
      (check-equal? (string<-request "foo" [42 "hello"] 13)
                    "{\"id\":13,\"jsonrpc\":\"2.0\",\"method\":\"foo\",\"params\":[42,\"hello\"]}"))
    '(test-case "decode result"
      (check-equal? (decode-json-rpc-response
                     1+ 69 (json<-string "{\"jsonrpc\": \"2.0\", \"result\": 1776, \"id\": 69}"))
                    1777))
    '(test-case "error equality"
      (check equal-object?
             (json-rpc-error code: -151 message: "foo" data: [1])
             (json-rpc-error code: -151 message: "foo" data: [1]))
      (check equal-object?
             (json-rpc-error code: -151 message: "foo")
             (json-rpc-error code: -151 message: "foo" data: (void))))))
