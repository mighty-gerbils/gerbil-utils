;; -*- Gerbil -*-
;;;; Utilities for concurrency

(export (except-out #t errorf warnf infof debugf verbosef))

(import
  :gerbil/gambit/bytes :gerbil/gambit/continuations
  :gerbil/gambit/random :gerbil/gambit/threads
  :std/actor :std/assert :std/error :std/format :std/logger
  :std/misc/bytes :std/misc/completion :std/misc/list :std/misc/repr :std/sugar
  ./base ./error ./exception)

(deflogger clan)

;;; Protocol for process shutdown: periodically check for the (shutdown?) flag,
;; so you can shutdown gracefully after having received a signal,
;; after you lost a race to find an answer, etc.
(def current-shutdown-predicate (make-parameter false))
(def (shutdown?) ((current-shutdown-predicate)))
(def (add-shutdown-predicate pred)
  (let ((previous (current-shutdown-predicate)))
    (current-shutdown-predicate
     (λ () (and (pred) (previous))))))


;;; Basic protocol for managing process trees.
;;; XXX vyzo: this is best accomplished with thread-groups
;;            see TAGS spawn/group and thread-group-kill!

;; hash-table used to represent a set. TODO: use actual set data structure
(def current-subprocesses
  (make-parameter (make-hash-table)))

(def (spawn/name/logged name function port: (port (current-error-port)))
  (spawn/name name (thunk-with-logged-exceptions function port: port)))

(def (spawn-subprocess name function . args)
  (let ((subprocess
         (spawn/name
          name
          (λ () (parameterize ((current-subprocesses (make-hash-table)))
                  (try
                   (apply function args)
                   (finally
                    (shutdown-all-subprocesses))))))))
    ;; NB: in the event of asynchronous interrupts, this updating of the current-subprocesses
    ;; should be atomically done after the process is spawned!
    (hash-put! (current-subprocesses) subprocess #t)
    subprocess))

(def (unregister-subprocess subprocess)
  (let ((subprocesses (current-subprocesses)))
    (assert! (hash-get subprocesses subprocess))
    (hash-remove! subprocesses subprocess)
    (void)))

(def (shutdown-subprocess subprocess)
  (unregister-subprocess subprocess)
  (!!rpc.shutdown subprocess))

(def (shutdown-all-subprocesses)
  (for-each shutdown-subprocess (hash-keys (current-subprocesses))))

;;; Function to indefinitely repeat a function call.
(def (indefinitely fun . args)
  (λ more-args
    (while #t
      (try
       (apply fun (append args more-args))
       (catch (e)
         (log-error "unhandled exception" e))))))


;;;; Sequentialize access to a (stateful) function
;; Same principle as Java's "synchronized", C#'s "lock".
;; We use an actor because it's simple in Gerbil.
;; We could have gone lower level and used a lock. Maybe to do later?
(defproto applicable
  (apply arguments))

(def (applicable-actor fun)
  (let loop ()
    (<- ((!applicable.apply arguments k)
         (try
          (!!value (apply fun arguments) k)
          (catch (e)
            (!!error e k)))
         (loop))
        ((!rpc.shutdown)
         (void)))))

;; TODO: does this leak resources when this wrapper is garbage collected but maybe not the actor?
(def (sequentialize/actor name fun)
  (let ((actor (spawn/name/logged name (lambda () (applicable-actor fun)))))
    (λ arguments (!!applicable.apply actor arguments))))

;; vyzo: this is a version of sequentialize that uses a plain mutex
;; + much more efficient
;; + easier to debug deadlocks
;; - downside is that the application context (thread and dynamic environment)
;;   is the caller's context.
(def (sequentialize/mutex name fun)
  (let (mx (make-mutex name))
    (lambda args
      (with-lock mx (cut apply fun args)))))

;; Choose which you use by default in your code
(def sequentialize (values sequentialize/mutex))

;;;; Race

;; Error raised when an exception was expected but a non-exceptional value was raised instead.
(defstruct (not-an-exception-error Exception) (value) transparent: #t)

;; Error raised when a shutdown message was received and no handler was found.
(defstruct (shutdown-error Exception) () transparent: #t)

(def (reify-exception thunk)
  (with-catch
   (λ (e) (if (exception? e) e (make-not-an-exception-error e)))
   (λ () (values->list (thunk)))))

(def (unreify-exception x)
  (cond
   ((list? x) (apply values x))
   ((exception? x) (raise x))))

;;;; Race between multiple forms, whereby the first to finish wins.
;; Forms may call a function to tell whether to shutdown yet (because someone else
;; finished faster); they may describe an actor that may also receive shutdown messages.

(defrules with-race ()
  ((_ (shutdown?) (name form) ...) (race/list [[name (λ (shutdown?) form)] ...])))

;; Return an A given a list of tasks that produce A, where each task is a tuple
;; of a name (any printable object) and a function that returns an A given a predicate
;; that returns true if some other task finished first and the current task should abort.
;; A <- (List (Tuple Any (A <- (Bool <-))))
(def (race/list task-list)
  (unreify-exception
   (thread-join!
    (spawn
     (λ ()
       (def referee (current-thread))
       (def done? #f)
       (def (shutdown?) done?)
       (def process<-fun
         (nest (λ-match) ([name fun])
               (spawn/name name) (λ ())
               (!!applicable.apply referee)
               (reify-exception) (λ ()) (fun shutdown?)))
       (def subprocesses (map process<-fun task-list))
       (def (shutdown!)
         (set! done? #t)
         (for-each (cut !!rpc.shutdown <>) subprocesses))
       (if (null? subprocesses)
         [(void)]
         (<- ((!applicable.apply arguments k)
              (shutdown!)
              arguments)
             (!rpc.shutdown
              (shutdown!)
              (make-shutdown-error)))))))))

;; Parallel version of map
(def (parallel-map f . ls)
  (map thread-join! (apply map (λ es (apply spawn f es)) ls)))

;; Parallel version of hash-value-map
(def (parallel-hash-value-map h f)
  (list->hash-table
   (parallel-map
    (λ-match ([k . v] (cons k (f v))))
    (hash->list h))))

;; Find a thread by name.
(def (primordial-thread-group)
  (thread-thread-group ##primordial-thread))

(def (all-threads (group (primordial-thread-group)))
  (with-list-builder (c)
    (let loop ((g group))
      (for-each c (thread-group->thread-list g))
      (for-each loop (thread-group->thread-group-list g)))))

(def (thread-named name)
  (find (λ (t) (equal? (thread-name t) name)) (all-threads)))

(def (all-threads-named name)
  (filter (lambda (t) (equal? (thread-name t) name)) (all-threads)))

;; Dump stack traces when things go wrong
(def (call-with-stack-trace-dumping thunk)
  (with-exception-stack-trace thunk))

(defrules with-stack-trace-dumping ()
  ((_ body ...) (call-with-stack-trace-dumping (λ () body ...))))

;; Run without interaction
(def (call-without-interaction thunk)
  (unhandled-actor-exception-hook-set! dump-stack-trace!)
  (try
   (call-with-stack-trace-dumping thunk)
   (catch (exn) (abort! 42 "Caught exception ~a" (repr exn)))))

(defrules without-interaction ()
  ((_ body ...) (call-without-interaction (λ () body ...))))

;; Join a list of threads. TODO: do it without blocking sequentially on each thread?
(def (join-threads! threads)
  (for-each thread-join! threads))

;; Retry a thunk until it either succeeds or fails,
;; with an exponential back off that is capped to constant back off.
;; max-retries: real number, in seconds, (+inf.0 for unlimited),
;;   maximum number of times to retry the action.
;; max-window: real number, in seconds, (+inf.0 for unlimited),
;;  maximum window within which to retry the action
;; retry-window: real number, in seconds,
;;  initial retry window, to be doubled at each retry, up to the max-window.
;; description: an object to print with repr, or #f
;;  a description to include in a log message. Nothing will be logged if it's #f.
(def (retry retry-window: retry-window
            max-window: max-window
            max-retries: max-retries
            description: (description #f)
            logging: (logging #f)
            thunk)
  (with-catch
   (lambda (e)
     (if (< 0 max-retries)
       (let* ((retry-window (min retry-window max-window))
              (sleep-duration (* (random-real) retry-window)))
         (when logging
           (logging "Sleeping %f seconds before to retry %r" sleep-duration description))
         (thread-sleep! sleep-duration)
         (retry retry-window: (* 2 retry-window)
                max-window: max-window
                max-retries: (1- max-retries)
                description: description
                thunk))
       (raise e)))
   thunk))

(def (simple-client send! make-message)
  (lambda (request)
    (def c (make-completion))
    (send! (make-message request (cut completion-post! c <>)))
    (cut completion-wait! c)))

(def (simple-server receive! process!)
  (while #t
    (match (receive!)
      ([input . continuation]
       (continuation (process! input))))))

(def (make-simple-client-server make-processor (name (##procedure-name make-processor)))
  (def (server-loop)
    (simple-server thread-receive (make-processor)))
  (def server (spawn/name/logged name server-loop))
  (simple-client (cut thread-send server <>) cons))

(def (completion-done val)
  (def c (make-completion))
  (completion-post! c val)
  c)

;; internal all-thread-parameters : [Parameterof [Listof Parameter]]
(def all-thread-parameters (make-parameter []))
;; call-with-re-parameterize : [Listof Parameter] [-> Any] -> Any
(def (call-with-re-parameterize ps f)
  (match ps
    ([] (f))
    ([p . rst] (parameterize ((p (p))) (call-with-re-parameterize rst f)))))

;; make-thread-parameter : A -> [Parameterof A]
;; Creates a parameter that is local to threads created by spawn/name/params
(def (make-thread-parameter v)
  (def p (make-parameter v))
  (all-thread-parameters (cons p (all-thread-parameters)))
  p)
;; spawn/name/params : Symbol [-> Any] port: OutputPort -> Thread
;; includes logged
;; Spawns a thread that makes sure thread-parameters are local to it
(def (spawn/name/params name function port: (port (current-error-port)))
  (spawn/name/logged
   name
   (lambda ()
     (call-with-re-parameterize (all-thread-parameters) function))
   port: port))
