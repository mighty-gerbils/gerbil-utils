(export json-rpc-test)

(import
  :std/misc/repr :std/sugar :std/test
  :clan/utils/maybe
  :clan/net/json-rpc)

(def json-rpc-test
  (test-suite "Tests for clan/net/json-rpc"
    (test-case "encode params"
      (check-equal? (string<-request "foo" [42 "hello"] 13)
                    "{\"jsonrpc\":\"2.0\",\"id\":13,\"method\":\"foo\",\"params\":[42,\"hello\"]}"))
    (test-case "decode result"
      (check-equal? (decode-response 1+ 69 "{\"jsonrpc\": \"2.0\", \"result\": 1776, \"id\": 69}")
                    1777))
    (test-case "decode error 1"
      (def response-str "{\"jsonrpc\": \"2.0\", \"error\": { \"code\": -151, \"message\": \"foo\", \"data\": [1] }, \"id\": 42 }")
      (check-equal? (with-catch identity (lambda () (decode-response 1+ 42 response-str) #f))
                    (json-rpc-error code: -151 message: "foo" data: [1])))
    (test-case "decode error 2"
      (def response-str "{\"jsonrpc\": \"2.0\", \"id\": 15 , \"error\": { \"code\": -32602, \"message\": \"non-array args\"}}\n")
      (check-equal? (with-catch identity (lambda () (decode-response 1+ 15 response-str) #f))
                    (json-rpc-error code: -32602 message: "non-array args" data: null)))
    (test-case "decode malformed 1"
      (def response-str "{\"jsonrpc\": \"2.0\", \"error\": 1776, \"id\": 42 }|}")
      (check-exception (decode-response 1+ 42 response-str) malformed-response?))
    (test-case "decode malformed 2"
      (def response-str "{\"jsonrpc\": \"2.0\", \"error\":  { \"code\": -1, \"message\": \"foo\" }, \"id\": 41 }")
      (check-exception (decode-response 1+ 42 response-str) malformed-response?))))
