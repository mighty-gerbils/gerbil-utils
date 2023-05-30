;; Support for JSON RPC 2.0 -- https://www.jsonrpc.org/specification

;; TODO: log details of http errors, e.g. 404 Not Found, etc.

(export #t)

(import
  :gerbil/gambit/ports :gerbil/gambit/exceptions
  :std/error :std/format :std/net/request :std/text/json :std/sugar
  ../base ../json ../maybe ./simple-http-client)

(defclass (json-rpc-error jsonable Exception)
  (code    ;; SInt16
   message ;; String
   data)   ;; (Maybe Bytes)
  transparent: #t constructor: :init!)
(defmethod {:init! json-rpc-error}
  (lambda (self code: code message: (message (void)) data: (data (void)))
    (class-instance-init! self code: code message: message data: data)))
(def (json-rpc-error<-json json)
  (trivial-object<-json json-rpc-error::t json))

(def json-rpc-version "2.0")

(defclass (json-rpc-request jsonable)
  (jsonrpc   ;; String, must be the same as json-rpc-version ("2.0"), can undefined for version 1.0
   method    ;; String
   params    ;; Json, array (arguments by position) or object (arguments by name)
   id)       ;; Json, MUST be an number, a string, or JSON null aka Scheme (void). SHOULD be an integer if a number. (void) if no response is required.
  transparent: #t)

(defclass (json-rpc-response jsonable)
  (jsonrpc   ;; String, must be the same as json-rpc-version ("2.0")
   result    ;; Json, JSON null (Scheme void) if there is an error
   error     ;; Json, JSON null (Scheme void) if there is no error
   id)       ;; Json, MUST be the same as provided in the request.
  transparent: #t
  constructor: :init!)

(defmethod {:init! json-rpc-response}
  (lambda (self jsonrpc: (jsonrpc (void))
           result: (result (void))
           error: (error (void))
           id: (id (void)))
    (class-instance-init! self jsonrpc: jsonrpc result: result error: error id: id)))

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

(defclass (malformed-request jsonable Exception) (method params e) transparent: #t)
(defclass (malformed-response jsonable Exception) (request-id response e) transparent: #t)

(def (decode-json-rpc-response decoder request-id response-json)
  (def (mal e)
    (raise (malformed-response request-id: request-id response: response-json e: (error-message e))))
  (def response (with-catch mal (cut trivial-object<-json json-rpc-response::t response-json)))
  (def jsonrpc (@ response jsonrpc))
  (def result (@ response result))
  (def error (@ response error))
  (def id (@ response id))
  (unless (or (void? jsonrpc) ;; a 1.0 server might fail to include this field
              (equal? jsonrpc json-rpc-version)) ;; but a recent server must reply with same version
    (mal "bad json_rpc_version"))
  (unless (or (void? result) (void? error))
    (mal "result error conflict"))
  (unless (equal? id request-id)
    (mal "bad id"))
  (if (void? error)
    (with-catch mal (cut decoder result))
    (raise (with-catch mal (cut json-rpc-error<-json error)))))

(def (string<-request method params id)
  (try (string<-json (json-rpc-request jsonrpc: json-rpc-version method: method params: params id: id))
       (catch (e) (raise (malformed-request method: method params: params e: (error-message e))))))

(def (json-rpc server-url method params
               auth: (auth #f)
               headers: (headers #f)
               cookies: (cookies #f)
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
    (request-response-bytes
     (http-post server-url
                auth: auth
                headers: `(("Content-Type" . "application/json") ,@headers)
                cookies: cookies
                data: (string->bytes request-string))))
  (def response-json
    (try (content->json response-bytes) ;; todo: move to decode-json-rpc-response ?
         (catch (e) (raise (malformed-response request-id: id response: response-bytes e: e)))))
  (when log
    (log [from: server-url response: (bytes->string response-bytes)]))

  (decode-json-rpc-response result-decoder
                   (and (hash-table? response-json) (hash-get response-json "id"))
                   response-json))
