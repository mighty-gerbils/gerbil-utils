;; -*- Gerbil -*-
;;;; Call limiter functionality
;; Respect limits of external APIs so we do not get banned.

(export
  define-call-limiter
  registered-limiters
  run-limiter-server
  serve-limiters
  sorted-limiters
  spawn-limiters
  unlimited-limiter
  use-limiter-server
  +limiter-server-address+
  )

;;#; ;; for debugging...
(export
  make-limiter
  call-limiter-loop)

(import
  :gerbil/gambit/threads
  :std/actor :std/format :std/logger :std/misc/list :std/misc/pqueue :std/sort
  :std/srfi/1 :std/srfi/19 :std/sugar
  :clan/utils/base :clan/utils/concurrency :clan/utils/date :clan/utils/multicall :clan/utils/number)

;;; Some infrastructure for call limiters:

(defproto limiter-service
  call:
  (get-ticket param) ;; ticket <- param
  event:
  (return-ticket ticket)) ;; bottom <- ticket

;; Given an call-limiter actor, a limiter-dependent parameter and a thunk,
;; execute the thunk while holding a ticket reserved from the call-limiter with given parameter.
(def (call-limiter server param fun . args)
  ;;(DBG "cl" server param fun args)
  (let ((ticket (!!limiter-service.get-ticket server param)))
    (match ticket
      (['sleep nanoseconds]
       ;; Sleep and retry after some time
       (sleep nanoseconds)
       (apply call-limiter server param fun args))
      (['ticket _ _ _]
       (try
        (apply fun args)
        (finally (!!limiter-service.return-ticket server ticket))))
      (_ (error "call-limiter mismatch" ticket server param fun args)))))

;; Given a limiter function, a name and some arguments,
;; run an actor message-processing loop for the call-limiter
;; with given name and rest of arguments,
;; that registers itself to the current rpc server.
(def (call-limiter-loop name . options)
  ;;(DBG "cll" name options)
  (defvalues (update-timestamp get-ticket return-ticket)
    (apply make-limiter name: name now: (current-monotonic-timestamp) options))
  (rpc-register (current-rpc-server) name)
  (let loop ()
    (<- ((!limiter-service.get-ticket param k)
         (try
          (let ((now (current-monotonic-timestamp)))
            (update-timestamp now)
            (!!value (get-ticket now param) k))
          (catch (e)
            (log-error "request error" e)
            (!!error (error-message e) k)))
         (loop))
        ((!limiter-service.return-ticket ticket)
         (let ((now (current-monotonic-timestamp)))
           (update-timestamp now)
           (return-ticket now ticket))
         (loop))
        ((!rpc.shutdown)
         (void)))))

(def +limiter-server-address+ (values "/tmp/limiter-server.sock"))

;; Abstracting over whether we use a limiter server or not (yes we do in production)
(def use-limiter-server (make-parameter #f))
(def registered-limiters (values '()))

;;;(import :clan/utils/debug)
;; Given a name, a limiter function and some arguments to that function,
;; lazily spawn an actor for that function with given name,
;; define a function of given name that consults either a local actor or a remote server.
(defrules define-call-limiter ()
  ((_ name options ...)
   (begin
     ;;(let ((n name) (opts [options ...]))
     ;;  (DBG "dcl" n opts))
     (bind-protocol! 'name limiter-service::proto)
     (defonce (limiter-actor)
       (spawn/name 'name call-limiter-loop 'name options ...))
     (push! ['name limiter-actor] registered-limiters)
     (def (limiter-end-point)
       (if (use-limiter-server)
         (rpc-connect (current-rpc-server) 'name +limiter-server-address+)
         (limiter-actor)))
     (def (name param fun . args) (apply call-limiter (limiter-end-point) param fun args)))))

(define-call-limiter unlimited-limiter
  max-tokens: 100
  period: one-nanosecond
  timeout: (* 1000 one-second)
  tokens-per-period: 100)

;; Return the list of limiters in sorted order.
;; (List (Tuple Symbol (Thread <-))) <-
(def (sorted-limiters)
  (sort registered-limiters (λ (x y) (apply string<? (map (compose symbol->string car) [x y])))))

;; Spawn the limiters in as many threads, return them in a list.
;; (List Thread) <- (List (Tuple Symbol (Thread <-)))
(def (spawn-limiters (limiters (sorted-limiters)))
  (map (λ-match ([name actor] (actor))) limiters))

(def (serve-limiters)
  (let ((sorted-limiters (sorted-limiters))
	(now (current-timestamp)))
    (printf "~a (~a) Serving the following limiters: ~a~%"
	    now (string<-timestamp now)
            (string-join (map (compose symbol->string car) sorted-limiters) " "))
    (for-each thread-join! (spawn-limiters sorted-limiters))))

;; Limiter server that ensures processes at current IP address don't overuse call limits
(define-entry-point (run-limiter-server (address +limiter-server-address+))
  "Run the limiter server that enforces exchange access limits"
  (current-rpc-server (start-rpc-server! +limiter-server-address+))
  (serve-limiters))

;; Given a number of tokens that regenerate only every given period,
;; make sure that every call waits enough before it is issued,
;; given how many tokens it costs.
;; See Kraken documentation
;; NB: This function supposes sequential execution.
;; You may use sequentialize to ensure that that's the case.
(def (make-limiter
      name: name
      now: (now (current-monotonic-timestamp))
      max-tokens: max-tokens
      tokens-per-period: tokens-per-period
      period: period-in-nanoseconds
      timeout: (timeout-in-nanoseconds (* 5 one-second))) ;; a ticket will expire after this timeout
  (def available-tokens max-tokens) ;; known to be available for use
  (def ticket-counter (make-counter 0)) ;; incremented every time a new ticket is issued
  (def outstanding-tickets (make-hash-table)) ;; : (Map [tokens: Integer timestamp: Integer] <- number: Integer)
  (def ticket-timeouts (make-pqueue car < max-tokens)) ;; : (PQueue [timestamp: Integer number: Integer tokens: Integer])
  (def token-respawns (make-pqueue car < max-tokens)) ;; : (PQueue [timestamp: Integer tokens: Integer])

  (def (update-timestamp now)
    (let loop () ;; First, handle tickets that timeout
      (unless (pqueue-empty? ticket-timeouts)
        (match (pqueue-peek ticket-timeouts)
          ([timestamp number tokens]
           (when (<= timestamp now)
             (pqueue-pop! ticket-timeouts)
             (let ((ticket [tokens timestamp])
                   (found (hash-get outstanding-tickets number)))
               (when found
                 (unless (equal? ticket found)
                   (error "ticket mismatch" name number ticket found))
                 (warning "~a ticket expired: ~s" name ticket)
                 (return-ticket timestamp ['ticket number tokens timestamp])))
             (loop)))
          (bogus (error "ticket-timeouts mismatch" name bogus)))))
    (let loop () ;; Next, handle tokens that respawn
      (unless (pqueue-empty? token-respawns)
        (match (pqueue-peek token-respawns)
          ([timestamp tokens]
           (when (<= timestamp now)
             (pqueue-pop! token-respawns)
             (let* ((periods-lapsed
                     (+ 1 (integer-part (/ (- now timestamp) period-in-nanoseconds))))
                    (tokens-returned (min tokens (* periods-lapsed tokens-per-period))))
               (increment! available-tokens tokens-returned)
               (decrement! tokens tokens-returned)
               (unless (zero? tokens)
                 (pqueue-push! token-respawns
                               [(+ timestamp (* periods-lapsed period-in-nanoseconds)) tokens])))
             (loop)))
          (bogus (error "token-respawns mismatch" name bogus))))))

  (def (get-ticket now cost)
    (cond
     ((<= cost available-tokens)
      (decrement! available-tokens cost)
      (let ((number (ticket-counter))
            (timeout (+ now timeout-in-nanoseconds)))
        (pqueue-push! ticket-timeouts [timeout number cost])
        (hash-put! outstanding-tickets number [cost timeout])
        ['ticket number cost timeout]))
     ((>= cost max-tokens)
      (error "request too costly"))
     (else
      (let ((next-respawn (car (pqueue-peek token-respawns))))
        ['sleep (- next-respawn now)]))))

  (def (return-ticket now ticket)
    (match ticket
      (['ticket number tokens timestamp]
       (let ((found (hash-get outstanding-tickets number)))
         (cond
          ((not found)
           (if (<= timestamp now)
             (warning "~a ticket returned after it expired: ~s" name ticket)
             (error "~a ticket returned but not found (double return?): ~s" name ticket)))
          ((not (equal? [tokens timestamp] found))
           (error "~a ticket returned but doesn't match!: ~s vs ~s" name ticket found))
          (else
           (hash-remove! outstanding-tickets number)
           (pqueue-push! token-respawns [(+ now period-in-nanoseconds) tokens])))))
      (_ (error "return-ticket" name ticket))))

  (values update-timestamp get-ticket return-ticket))
