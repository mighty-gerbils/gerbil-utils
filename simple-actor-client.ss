;; -*- Gerbil -*-
;;;; Module to import in a simple actor client that isn't also a server.
;; Importing this module into the main module of an executable will
;; setup basic environment variables to enables RPC as a client.
;; If you want to export actor as a server, you'll need more setup.

(export #t)

(import :std/actor :std/logger :std/text/json)

(def (ensure-actor-client!)
  (cond
   ((current-actor-server) => values)
   (else
    ;; Start a daemon without a listening address,
    ;; and configure the c-r-s dynamic parameter.
    ;; NB: start-rpc-server! already calls start-logger! so we don't have to.
    ;; NB: if/when we have asynchronous interrupts, shouldn't this be atomic?
    (current-actor-server (start-actor-server!)))))
