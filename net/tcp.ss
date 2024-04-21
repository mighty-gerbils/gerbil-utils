(export tcp-listener?
        tcp-listen
        tcp-client-port?
        tcp-connect
        tcp-accept
        tcp-close
        try-tcp-connect
        tcp-connect/retry)

(import :gerbil/gambit
        :std/error
        (only-in :scheme/base u8-ready?)
        :std/format :std/pregexp :std/sugar
        :clan/base
        (only-in :clan/concurrency retry/function)
        :clan/timestamp)

;; --------------------------------------------------------

;; Listeners and connections with u8-ready

;; tcp-connection-ready? : Tcp-Client-Port -> Bool
(def (tcp-connection-ready? port)
  (and (tcp-client-port? port)
       (try (##wait-output-port port)
            (u8-ready? port)
         (catch (os-exception:tcp-client-port? e) #f))))

;; A Tcp-Listener is a (tcp-listener Tcp-Server-Port)
(defstruct tcp-listener (port))

;; tcp-listen : PortAddressSettings -> Tcp-Listener
(def (tcp-listen port-addr-settings)
  (tcp-listener (open-tcp-server port-addr-settings)))

;; tcp-connect : PortAddressSettings -> Tcp-Client-Port
(def (tcp-connect port-addr-settings)
  (def port (open-tcp-client port-addr-settings))
  (unless (tcp-connection-ready? port) (error "tcp-connect: connection not ready"))
  port)

;; tcp-accept : Tcp-Listener -> Tcp-Client-Port
(def (tcp-accept listener)
  (def port (read (tcp-listener-port listener)))
  (unless (tcp-connection-ready? port) (error "tcp-accept: connection not ready"))
  port)

;; tcp-close : Tcp-Listener -> Void
(def (tcp-close listener)
  (close-port (tcp-listener-port listener)))

;; try-tcp-connect : PortAddressSettings (-> Fail) -> Tcp-Client-Port | Fail
(def (try-tcp-connect port-addr-settings failure)
  (def port (try-open-tcp-client port-addr-settings (lambda () #f)))
  (cond
    ((not port) (failure))
    ((tcp-connection-ready? port) port)
    (else (ignore-errors (close-port port)) (failure))))

(def (tcp-connect/retry retry-window: retry-window
                        max-window: max-window
                        max-retries: max-retries
                        port-addr-settings
                        failure)
  (retry/function
    retry-window: retry-window
    max-window: max-window
    max-retries: max-retries
    (lambda (failure) (try-tcp-connect port-addr-settings failure))
    failure))

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
