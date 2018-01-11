;; -*- Gerbil -*-
;;;; Watching and restarting OS-spawned subprocesses
;;
;; TODO:
;; V1:
;; Store the PID in a watch file
;; Don't worry too much about atomicity, there will be a human operator watching
;;
;; V2:
;; Have a poor man's ORB store the communication end-points and PIDs in a SQLite DB.
;; Use two-phase commit so the new process only actually starts after its PID is registered.
;; When gracefully killing an old process, minimize the time during which neither is active.
;;
;; V3:
;; Go get Erlang documentation, do the same things that they do.
;; Make sure that spawning and registering actors in the "process tree" is atomic.
;; Have an ORB for services to find each other? Use the actual Erlang protocol? dbus or similar?
;;
;; V4:
;; Have a migration protocol so the old process can migrate state (snapshot + increments)
;; to the new one before the new one becomes master and it dies, ensuring no connection is lost.
;;

(export #t)

(import
  :gerbil/gambit/ports :gerbil/gambit/threads
  :std/actor :std/format :std/logger :std/misc/list :std/misc/ports :std/misc/process
  :std/pregexp :std/srfi/13 :std/sugar
  :utils/base :utils/basic-parsers :utils/date
  :utils/ffi :utils/files :utils/json :utils/list :utils/logger :utils/path-config)

;; Class Daemon-Status-Register
(defclass daemon-status-register ())

;; : <- Daemon-Status-Register Daemon-Status
(defmethod {write daemon-status-register} undefined)

;; : Daemon-Status <- Daemon-Status-Register
(defmethod {read daemon-status-register} undefined)


;; Store the status in a file.
;; Class Trivial-Daemon-Status-Register
(defclass (trivial-daemon-status-register daemon-status-register)
  (path))
(defmethod {write trivial-daemon-status-register}
  (λ (self status) (write-file-json (@ self path) status)))
(defmethod {read trivial-daemon-status-register}
  (λ (self) (read-file-json (@ self path))))


;; Schema for status:
;; "previous" => previous process PID
;; "current" => current process PID

(defonce (daemon-watch-logger) (json-logger "daemon-watch" top: (run-directory)))

;; Kill a daemon of given pid.
;; But first check that the PID is still assigned to a relevant daemon process,
;; by calling the daemon-pid? predicate, so we don't kill a random process with same PID.
;; First kill with -1 (SIGHUP). Then Give the process a 5 second grace delay before killing with kill -9.
(def (kill-daemon name pid (daemon-pid? true))
  (def (kill-it signal message)
    ((daemon-watch-logger) [message pid (command-line<-pid pid)])
    (kill pid signal))

  (cond
   ((not pid)
    (void))
   ((not (daemon-pid? pid))
    ((daemon-watch-logger)
     ["Not killing" name pid (command-line<-pid pid)]))
   (else
    (kill-it SIGTERM "Daemon please die (SIGTERM)")
    (spawn/name
     ['coup-de-grace pid]
     (λ ()
       (sleep (* 5 one-second))
       (when (daemon-pid? pid)
         (kill-it SIGKILL "Die vile daemon scum (SIGKILL)"))))))
  (void))

(def (process-running? process)
  (not (ignore-errors (process-status process))))

(def (daemon-watcher
      name: name
      launch-daemon: launch-daemon
      daemon-status-register: daemon-status-register
      grace-period: grace-period ;; time after startup that the process isn't watched
      watch-period: watch-period ;; time after which to test again
      daemon-pid?: daemon-pid? ;; does the PID still refer to the daemon?
      daemon-healthy?: healthy?) ;; is the daemon process healthy?

  ;; TODO: have some locking mechanism
  (def current-status (or (ignore-errors {read daemon-status-register}) (hash)))
  (kill-daemon name (hash-get current-status "previous") daemon-pid?)
  (kill-daemon name (hash-get current-status "current") daemon-pid?)
  ;; TODO: have a handshake so we only kill the present when the new one is ready
  ;; TODO: make the current one the previous one after you launch the new one
  ;; but before you complete the handshake.
  ;; TODO: make it an asynchronous actor that can immediately sense process death, etc.
  (while #t
    (let* ((process (launch-daemon))
           (pid (process-pid process)))
      (try
       {write daemon-status-register (hash ("current" pid))}
       ((daemon-watch-logger) ["I summon thee daemon" name pid (command-line<-pid pid)])
       (sleep grace-period)
       (while (and (process-running? process) (healthy? process))
         (sleep watch-period))
       (kill-daemon name pid daemon-pid?)
       (finally
        (close-port process))))))

(def (read-null-delimited-string-list port)
  (nest
   (and port)
   (with-list-builder (c))
   (let loop ())
   (let ((arg (read-line port (integer->char 0)))))
   (unless (eof-object? arg)
     (c arg)
     (loop))))

(def (read-integer-list port)
  (nest
   (and port)
   (with-list-builder (c))
   (let loop ()
     (expect-and-skip-any-whitespace port))
   (unless (eof-object? (peek-char port))
     (c (expect-natural port))
     (loop))))

;; TODO: make it portable beyond Linux. At least make it error out outside Linux.
;; TODO: is ignore-errors working? Should we use it?
(def (command-line<-pid pid)
  (ignore-errors
   (call-with-input-file [path: (format "/proc/~d/cmdline" pid)]
     (λ (port)
       (and port (read-null-delimited-string-list port))))))

;; TODO: find a home for this function
(def (string-all-digits? s)
  (string-every char-numeric? s))

;; Linux specific!
(def (all-pids)
  (map string->number (filter string-all-digits? (directory-files "/proc"))))

;; Linux specific!
(def (pid-statm pid)
  (ignore-errors
   (call-with-input-file [path: (format "/proc/~d/statm" pid)] read-integer-list)))

;; Linux specific!
(def (pid-fds pid)
  (ignore-errors
   (map string->number (filter string-all-digits? (directory-files (format "/proc/~d/fd" pid))))))

;; Linux specific!
(def (meminfo)
  (map (λ (line) (match (pregexp-match "^([A-Za-z0-9_()]+): +([0-9]+)(:? kB)?$" line)
                   ([_ name num unit] [name (string->number num)])))
       (read-file-lines "/proc/meminfo")))
