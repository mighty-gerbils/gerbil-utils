;; -*- Gerbil -*-
;;;; Simple HTTP client code
;; Create a query string URL, get JSON from it.

(export #t)

(import
  :gerbil/gambit/ports
  :std/net/request :std/text/json :std/sugar
  ../base)

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

(def (content->json content)
  (and content
       (parameterize ((json-symbolic-keys #f)) ;; Don't intern JSON keys
         (with-input-from-u8vector [char-encoding: 'UTF-8 init: content]
                                   read-json))))

(def (http-get-content url)
  (and url (request-response-bytes (http-get url))))

(def (request-response-bytes req)
  (try
   (if (eq? (request-status req) 200)
     (request-content req)
     (error "HTTP request failed" (request-status req) (request-status-text req)))
   (finally
    (request-close req))))
