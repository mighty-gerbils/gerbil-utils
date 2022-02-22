
(export tcp-listener?
        tcp-listen
        tcp-client-port?
        tcp-connect
        tcp-accept
        tcp-close
        try-tcp-connect
        tcp-connect/retry-until-deadline)

(import :gerbil/gambit/exceptions :gerbil/gambit/ports :gerbil/gambit/threads
        :std/format :std/pregexp :std/sugar
        :clan/base :clan/timestamp)

;; --------------------------------------------------------

;; Listeners and connections with SYN/ACC

(def SYN 22)
(def ACK 6)
(def (syn/ack port)
  (write-u8 SYN port)
  (force-output port)
  (and
    (equal? (read-u8 port) SYN)
    (begin
      (write-u8 ACK port)
      (force-output port)
      (equal? (read-u8 port) ACK))))

;; A Tcp-Listener is a (tcp-listener Tcp-Server-Port)
(defstruct tcp-listener (port))

;; tcp-listen : PortAddressSettings -> Tcp-Listener
(def (tcp-listen port-addr-settings)
  (tcp-listener (open-tcp-server port-addr-settings)))

;; tcp-connect : PortAddressSettings -> Tcp-Client-Port
(def (tcp-connect port-addr-settings)
  (def port (open-tcp-client port-addr-settings))
  (unless (syn/ack port) (error "tcp-connect: SYN/ACK failed"))
  port)

;; tcp-accept : Tcp-Listener -> Tcp-Client-Port
(def (tcp-accept listener)
  (def port (read (tcp-listener-port listener)))
  (unless (syn/ack port) (error "tcp-accept: SYN/ACK failed"))
  port)

;; tcp-close : Tcp-Listener -> Void
(def (tcp-close listener)
  (close-port (tcp-listener-port listener)))

;; try-tcp-connect : PortAddressSettings (-> Fail) -> Tcp-Client-Port | Fail
(def (try-tcp-connect port-addr-settings failure)
  (def port (try-open-tcp-client port-addr-settings (lambda () #f)))
  (cond
    ((not port) (failure))
    ((try (syn/ack port) (catch (os-exception:tcp-client-port? e) #f)) port)
    (else (ignore-errors (close-port port)) (failure))))

(def (tcp-connect/retry-until-deadline port-addr-settings deadline failure)
  (let loop ()
    (try-tcp-connect port-addr-settings
      (lambda ()
        (cond ((> (current-unix-time) deadline) (failure))
              (else (thread-sleep! 1) (loop)))))))

;; --------------------------------------------------------

;; Gambit's tcp client ports

;; from gambit lib/_io#.scm
;; definition of macro-tcp-client-kind
(def TCP_CLIENT_PORT_KIND (+ 31 512))

;; try-open-tcp-client : PortAddressSettings (-> Fail) -> Tcp-Client-Port | Fail
(def (try-open-tcp-client addr-info failure)
  (try
    (open-tcp-client addr-info)
    (catch (os-exception:tcp-client-port? e) (failure))))

;; os-exception:tcp-client-port? : Any -> Bool
(def (os-exception:tcp-client-port? e)
  (and (os-exception? e)
       (ormap tcp-client-port? (os-exception-arguments e))))

;; tcp-client-port? : Any -> Bool
(def (tcp-client-port? v)
  (and (input-port? v)
       (output-port? v)
       (##port-of-kind? v TCP_CLIENT_PORT_KIND)))

;; --------------------------------------------------------
