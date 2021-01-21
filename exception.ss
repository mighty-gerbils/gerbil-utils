(export
  string<-exception
  with-catch/cont
  with-logged-exceptions
  call-with-logged-exceptions
  thunk-with-logged-exceptions)

(import
  :gerbil/gambit/continuations :gerbil/gambit/exceptions :gerbil/gambit/threads
  :std/format :std/sugar)

(def (string<-exception e)
  (call-with-output-string (cut display-exception e <>)))

;; The exception and continuation are valid for use with display-exception-in-context
;; and display-continuation-backtrace
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

(def (call-with-logged-exceptions thunk port: (port (current-error-port)))
  (with-catch/cont
   (lambda (e k)
     (fprintf port "In thread ~a:\n" (thread-name (current-thread)))
     (display-exception-in-context e k port)
     (display-continuation-backtrace k port #t #t 20 20)
     (raise e))
   thunk))

(def (thunk-with-logged-exceptions thunk port: (port (current-error-port)))
  (lambda () (call-with-logged-exceptions thunk port: port)))

(defrule (with-logged-exceptions (options ...) body ...)
  (call-with-logged-exceptions (lambda () body ...) options ...))

;; TODO: for end-user reporting, use error contexts as ported from quux or cl-scripting.
;; Maybe also port that to Gerbil main, and use it in std/test ?
