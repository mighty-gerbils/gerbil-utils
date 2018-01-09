;; -*- Gerbil -*-
;;;; Utilities for using websocket

(export #t)

(import
  :gerbil/gambit/threads
  :std/actor :std/error :std/logger :std/net/websocket :std/text/json :std/sugar
  :utils/base)

(defproto websocket-client
  event: ;; asynchronous
  (receive msg)
  (send msg)
  (close-connection e))

(def (websocket-decode-message-json bytes type)
  (or (and (eq? type 'text)
           (ignore-errors
            (read-json (open-input-u8vector [char-encoding: 'UTF-8 init: bytes]))))
      (begin
        (warning "websocket: server sent binary data (~s bytes)" (u8vector-length bytes))
        (raise-io-error 'websocket "server sent binary data" bytes))))

(def (websocket-encode-message-json msg)
  (let (outp (open-output-u8vector))
    (write-json msg outp)
    (values (get-output-u8vector outp) 'text)))

(def (websocket-client-controller
      url
      handle-message: handle-message
      shutdown: (shutdown void)
      connect-limiter: (connect-limiter funcall)
      send-limiter: (send-limiter funcall)
      decode-message: (decode-message websocket-decode-message-json)
      encode-message: (encode-message websocket-encode-message-json))

  ;; Start websocket client
  (def client (connect-limiter open-websocket-client url))
  (def controller (current-thread))
  (def receiver (spawn receiver-loop))

  (def (receive-message)
    (let ((values bytes type) (websocket-recv client))
      (decode-message bytes type)))

  (def (send-message message)
    (let ((values bytes type) (encode-message message))
      (websocket-send client bytes type)))

  (def (receiver-loop)
    (try
     (let loop ()
       (!!websocket-client.receive controller (receive-message))
       (loop))
     (catch (e)
       (!!websocket-client.close-connection controller e))))

  (try
   (let loop ()
     (<- ((!websocket-client.receive message)
          (handle-message message)
          (loop))
         ((!websocket-client.send message)
          (send-limiter send-message message)
          (loop))
         ((!websocket-client.close-connection e)
          (raise e))
         ;; control
         ((!rpc.shutdown)
          (shutdown))
         (bogus
          (error "unexpected message: ~s" bogus))))
   (finally
    (websocket-close client))))
