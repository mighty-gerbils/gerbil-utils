;; -*- Gerbil -*-
;;;; Simple HTTP client code
;; Create a query string URL, get JSON from it.

(export #t)

(import
  :gerbil/gambit/ports
  :std/net/request :std/text/json :std/sugar
  :utils/base)

;; Create a query string...
;; BEWARE!!! This does NO VALIDATION of the command and option syntax.
(def (query-string command . options)
  (call-with-output-string
    '()
    (λ (o)
      (display command o)
      (let loop ((options options)
                 (separator "?"))
        (match options
          ('() (void))
          ([key value . more]
           (if value
             (begin
               (display separator o)
               (display key o)
               (display "=" o)
               (display value o)
               (loop more "&"))
             (loop more separator))))))))

;; This function is extracted from parts of std/net/request#request-text
;; Move that to the gerbil std lib, have request-text use it.
(def (bytes->string bytes (encoding #f))
  (let* ((in   (open-input-u8vector [init: bytes char-encoding: (or encoding 'UTF-8)]))
         (len  (u8vector-length bytes))
         (out  (make-string len))
         (len  (read-substring out 0 len in)))
    (string-shrink! out len)
    out))

(def (content->json content)
  (and content
       (parameterize ((json-symbolic-keys #f)) ;; Don't intern JSON keys
         (with-input-from-u8vector [char-encoding: 'UTF-8 init: content]
                                   read-json))))

(def (http-get-content url)
  (and url
       (let (req (http-get url))
         (try
          (if (eq? (request-status req) 200)
            (request-content req)
            (error "HTTP request failed" (request-status req) (request-status-text req)))
          (finally
           (request-close req))))))
