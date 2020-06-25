;; Support for JSON RPC 2.0 -- https://www.jsonrpc.org/specification

(export #t)

(import
  :gerbil/gambit/ports :gerbil/gambit/exceptions
  :std/error :std/format :std/net/request :std/text/json :std/sugar
  ../utils/base ../utils/json ../utils/maybe ../net/simple-http-client)

(defclass (json-rpc-error jsonable)
  (code    ;; SInt16
   message ;; String
   data)   ;; (Maybe Bytes)
  transparent: #t constructor: :init!)
(defmethod {:init! json-rpc-error}
  (lambda (self code: code message: message data: (data null))
    (class-instance-init! self code: code message: message data: data)))

(def json-rpc-version "2.0")

(defclass (json-rpc-request jsonable)
  (jsonrpc   ;; String, must be the same as json-rpc-version ("2.0")
   method    ;; String
   params    ;; Json, array (arguments by position) or object (arguments by name)
   id)       ;; Json, MUST be an number, a string, or Null. SHOULD be an integer if a number. Null if no response is required.
  transparent: #t)

;; Type of a successful JSON-RPC response.
(defclass (result-response jsonable)
  (jsonrpc ;; String, must be the same as json-rpc-version ("2.0")
   result  ;; Json
   id)     ;; Json, must match that of the request
  transparent: #t)

;; Type of a failed JSON-RPC response.
(defclass (error-response jsonable)
  (jsonrpc ;; string
   error   ;; error
   id)     ;; json
  transparent: #t)

;; Global counter to correlate responses and answers in logs.
(def id-counter
  (let (counter 0)
    (lambda ((increment 1))
      (begin0 counter (set! counter (+ counter increment))))))

(def rpc-timeout 10) ;; in seconds --- where is that from?
(def rpc-log #t) ;; do we want a parameter?

(def (parser-error e)
  (json-rpc-error code: -32700
                  message: "An error occurred on the server while parsing the JSON text."
                  e: (error-message e)))

(def (invalid-request request)
  (json-rpc-error code: -32600
                  message: "The JSON sent is not a valid Request object."
                  e: request))

(def (method-not-found name)
  (json-rpc-error code: -32601
                  message: "The method does not exist / is not available."
                  e: name))

(def (invalid-params params)
  (json-rpc-error code: -32602
                  message: "Invalid method parameter(s)."
                  e: params))

;; Beware: avoid leaking too much internal information with the message, put the full monty in logs.
(def (internal-error exn)
  (json-rpc-error code: -32603
                  message: "Internal JSON-RPC error."
                  e: (error-exception-message exn)))

(def (application-error m e) (json-rpc-error code: -32500 message: m e: e))
(def (system-error m e) (json-rpc-error code: -32400 message: m e: e))
(def (tranport-error m e) (json-rpc-error code: -32300 message: m e: e))

(defclass (malformed-request jsonable exception) (method params e) transparent: #t)
(defclass (malformed-response jsonable exception) (request-id response e) transparent: #t)

(def (decode-json-rpc-response decoder request-id response-json)
  (def (mal e)
    (raise (malformed-response request-id: request-id response: response-json e: (error-message e))))
  (def (checking jsonrpc x id)
    (cond
     ((not (equal? jsonrpc json-rpc-version))
      (mal "bad json_rpc_version"))
     ((not (equal? id request-id))
      (mal "bad id"))
     (else x)))
  (if (hash-key? response-json "result")
    (let (result-response-json
          (with-catch mal (cut trivial-object<-json result-response::t response-json)))
      (match result-response-json
        ((result-response jsonrpc: jsonrpc result: result-json id: id)
         (let (result-json (checking jsonrpc result-json id))
           (with-catch mal (cut decoder result-json))))))
    (let (error-response-json
          (with-catch mal (cut trivial-object<-json error-response::t response-json)))
      (match error-response-json
        ((error-response jsonrpc: jsonrpc error: error-json id: id)
         (let (error-json (checking jsonrpc error-json id))
           (raise (with-catch mal (cut trivial-object<-json json-rpc-error::t error-json)))))))))

(def (string<-request method params id)
  (try (string<-json (json-rpc-request jsonrpc: json-rpc-version method: method params: params id: id))
       (catch (e) (raise (malformed-request method: method params: params e: (error-message e))))))

(def (json-rpc server-url method params
               result-decoder: (result-decoder identity)
               param-encoder: (param-encoder identity)
               timeout: (timeout rpc-timeout)
               log: (log #f))
  (def id (id-counter))
  (def request-string (string<-request method (param-encoder params) id))
  (when log
    (log [to: server-url request: request-string]))

  ;; TODO: implement timeouts, with semi-asynchronous aborts the http-post thread itself.
  (def response-bytes
    (let (req (http-post server-url
                         headers: '(("Content-Type" . "application/json"))
                         data: (string->bytes request-string)))
      (try
       (if (equal? (request-status req) 200)
         (request-content req)
         (error "HTTP request failed" (request-status req) (request-status-text req)))
       (finally
        (request-close req)))))
  (def response-json
    (try (content->json response-bytes) ;; todo: move to decode-json-rpc-response ?
         (catch (e) (raise (malformed-response request-id: id response: response-bytes e: e)))))
  (when log
    (log [from: server-url response: (bytes->string response-bytes)]))

  (decode-json-rpc-response result-decoder
                   (and (hash-table? response-json) (hash-get response-json "id"))
                   response-json))
