;; -*- Gerbil -*-
;;;; Utilities for concurrency

(export #t)

(import
  :gerbil/gambit/continuations :gerbil/gambit/threads
  :std/actor :std/error :std/logger :std/misc/list :std/misc/repr :std/sugar
  :clan/utils/base :clan/utils/error)

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
(def (sequentialize name fun)
  (let ((actor (spawn/name name applicable-actor fun)))
    (λ arguments (!!applicable.apply actor arguments))))


;;;; Race

;; Error raised when an exception was expected but a non-exceptional value was raised instead.
(defstruct (not-an-exception-error <error>) (value))

;; Error raised when a shutdown message was received and no handler was found.
(defstruct (shutdown-error <error>) ())

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
