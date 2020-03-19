(export with-catch/cont)

;; with-catch/cont : [Exception Continuation -> A] [-> A] -> A
(def (with-catch/cont handler thunk)
  (let/esc outside
    (def E (current-exception-handler))
    (def (escaping-handler exn)
      (##continuation-capture
       (lambda (inside)
         (with-exception-handler E
          (lambda ()
            (outside (handler exn inside)))))))
    (with-exception-handler escaping-handler thunk)))
