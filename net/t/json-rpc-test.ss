(export json-rpc-test)

(import
  :std/misc/repr :std/sugar :std/test :std/text/json
  :clan/base :clan/json :clan/maybe
  ../json-rpc)

(def json-rpc-test
  (test-suite "test suite for clan/net/json-rpc"
    (test-case "encode params"
      (check-equal? (string<-request "foo" [42 "hello"] 13)
                    "{\"jsonrpc\":\"2.0\",\"id\":13,\"method\":\"foo\",\"params\":[42,\"hello\"]}"))
    (test-case "decode result"
      (check-equal? (decode-json-rpc-response
                     1+ 69 (json<-string "{\"jsonrpc\": \"2.0\", \"result\": 1776, \"id\": 69}"))
                    1777))
    (test-case "error equality"
      (check equal-object?
             (json-rpc-error code: -151 message: "foo" data: [1])
             (json-rpc-error code: -151 message: "foo" data: [1]))
      (check equal-object?
             (json-rpc-error code: -151 message: "foo")
             (json-rpc-error code: -151 message: "foo" data: (void))))
    (test-case "decode error 1"
      (def response-json
        (json<-string "{\"jsonrpc\": \"2.0\", \"error\": { \"code\": -151, \"message\": \"foo\", \"data\": [1] }, \"id\": 42 }"))
      (check-exception (decode-json-rpc-response 1+ 42 response-json)
                       (cut equal-object? <> (json-rpc-error code: -151 message: "foo" data: [1]))))
    (test-case "decode error 2"
      (def response-json
        (json<-string "{\"jsonrpc\": \"2.0\", \"id\": 15 , \"error\": { \"code\": -32602, \"message\": \"non-array args\"}}\n"))
      (check-exception (decode-json-rpc-response 1+ 15 response-json)
                       (cut equal-object? <> (json-rpc-error code: -32602 message: "non-array args"))))
    (test-case "decode malformed 1"
      (def response-json
        (json<-string
         "{\"jsonrpc\": \"2.0\", \"error\": 1776, \"id\": 42 }|}"))
      (check-exception (decode-json-rpc-response 1+ 42 response-json) malformed-response?))
    (test-case "decode malformed 2"
      (def response-json
        (json<-string
         "{\"jsonrpc\": \"2.0\", \"error\":  { \"code\": -1, \"message\": \"foo\" }, \"id\": 41 }"))
      (check-exception (decode-json-rpc-response 1+ 42 response-json) malformed-response?))))
