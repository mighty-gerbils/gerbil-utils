
(export tcp-integrationtest)

(import :gerbil/gambit
        :std/iter :std/sugar :std/test :std/text/json
        :clan/concurrency :clan/json :clan/timestamp
        ../tcp)

(def tcp-integrationtest
  (test-suite "test suite for clan/net/tcp"
    (test-case "tcp line chat"
      (def (write-line str out)
        (display str out)
        (newline out)
        (force-output out))

      ;; start client first to test retry
      (def client
        (spawn/name/logged
          'client
          (lambda ()
            (def port
              (tcp-connect/retry [address: "localhost" port-number: 8000] (lambda () #f)
                                 retry-window: 1
                                 max-window: 10
                                 max-retries: 10))
            (check-predicate port tcp-client-port?)
            (write-line "hello" port)
            (check-equal? (read-line port) "hi")
            (write-line "fruit?" port)
            (check-equal? (read-line port) "no")
            (check-equal? (read-line port) "bread?")
            (write-line "no" port)
            (write-line "fruit & bread?" port)
            (check-equal? (read-line port) "probably not")
            (check-equal? (read-line port) "do you like rectangles?")
            (write-line "mmm, my favorite thing to have for breakfast!" port)
            (check-equal? (read-line port) "yeah")
            (write-line "okay" port)
            (write-line "bye" port)
            (close-output-port port)
            (check-predicate (read-line port) eof-object?)
            (close-input-port port)
            #t)))

      ;; have server sleep to test retry
      (def server
        (spawn/name/logged
          'server
          (lambda ()
            (thread-sleep! 1)
            (def listener (tcp-listen [local-port-number: 8000]))
            (check-predicate listener tcp-listener?)
            (def port (tcp-accept listener))
            (check-predicate port tcp-client-port?)
            (check-equal? (read-line port) "hello")
            (write-line "hi" port)
            (check-equal? (read-line port) "fruit?")
            (write-line "no" port)
            (write-line "bread?" port)
            (check-equal? (read-line port) "no")
            (check-equal? (read-line port) "fruit & bread?")
            (write-line "probably not" port)
            (write-line "do you like rectangles?" port)
            (check-equal? (read-line port) "mmm, my favorite thing to have for breakfast!")
            (write-line "yeah" port)
            (check-equal? (read-line port) "okay")
            (check-equal? (read-line port) "bye")
            (close-output-port port)
            (check-predicate (read-line port) eof-object?)
            (close-input-port port)
            (tcp-close listener)
            #t)))
      (check-equal? (thread-join! client) #t)
      (check-equal? (thread-join! server) #t))

    (test-case "tcp json countdown"
      ;; start client first to test retry
      (def client
        (spawn/name/logged
          'client
          (lambda ()
            (def port
              (tcp-connect/retry [address: "localhost" port-number: 8000] (lambda () #f)
                                 retry-window: 1
                                 max-window: 10
                                 max-retries: 10))
            (write-json-ln (hash (C 1) (E 3) (G 5)) port)
            (write-json-ln (hash (B 7) (D 2) (G 5)) port)
            (write-json-ln (hash (C 1) (E 3) (A 6)) port)
            (write-json-ln (hash (C 1) (F 4) (A 6)) port)
            (write-json-ln (hash (C 1) (E 3) (G 5) (B 7)) port)
            (force-output port)
            (close-output-port port)
            (begin0
              (read-all port read-json)
              (close-input-port port)))))

      ;; have server sleep to test retry
      (def server
        (spawn/name/logged
          'server
          (lambda ()
            (thread-sleep! 1)
            (def listener (tcp-listen [local-port-number: 8000]))
            (def port (tcp-accept listener))
            (def js (read-all port read-json))
            (for ((j js)
                  (i (in-range (1- (length js)) -1 -1)))
                (write-json-ln [i j] port))
            (force-output port)
            (close-port port)
            (tcp-close listener))))
      (thread-join! server)
      (check-equal? (json-object->string (thread-join! client))
                    (json-object->string
                     [[4 (hash (C 1) (E 3) (G 5))]
                      [3 (hash (B 7) (D 2) (G 5))]
                      [2 (hash (C 1) (E 3) (A 6))]
                      [1 (hash (C 1) (F 4) (A 6))]
                      [0 (hash (C 1) (E 3) (G 5) (B 7))]])))))
