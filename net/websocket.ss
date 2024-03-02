;; -*- Gerbil -*-
;;;; Utilities for using websocket
(import
  (only-in :std/actor defmessage <- -> @shutdown @ping @unexpected)
  (only-in :std/contract using)
  (only-in :std/error raise-io-error)
  (only-in :std/logger warnf)
  (only-in :std/net/websocket message websocket-connect
           WebSocket-recv WebSocket-send WebSocket-close)
  (only-in :std/sugar try catch finally while ignore-errors)
  (only-in :std/text/json read-json write-json)
  (only-in :clan/base funcall))

(defmessage !receive (msg))
(defmessage !send (msg))
(defmessage !close-connection (e))

(def (websocket-decode-message-json bytes type)
  (or (and (eq? type 'text)
           (ignore-errors
            (read-json (open-input-u8vector [char-encoding: 'UTF-8 init: bytes]))))
      (begin
        (warnf "websocket: server sent binary data (~s bytes)" (u8vector-length bytes))
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
  (def client (connect-limiter websocket-connect url))
  (def controller (current-thread))
  (def receiver (spawn receiver-loop))

  (def (receive-message)
    (using (msg (WebSocket-recv client) :- message)
      (decode-message msg.data msg.type)))

  (def (send-message message)
    (let ((values data type) (encode-message message))
      (WebSocket-send client (message data type))))

  (def (receiver-loop)
    (try
     (while #t
       (-> controller (!receive (receive-message))))
     (catch (e)
       (-> controller (!close-connection e)))))

  (try
   (let/cc exit
     (while #t
       (<- ((!receive message)
            (handle-message message))
           ((!send message)
            (send-limiter send-message message))
           ((!close-connection e)
            (raise e))
           ;; control
           ,(@shutdown exit)
           ,(@ping)
           ,(@unexpected warnf)
          (shutdown))))
   (finally
    (WebSocket-close client))))
