;; -*- Gerbil -*-
;;;; General-purpose logging facility.
;; TODO: modify the interface to allow for actions on log rotation.

(export
  #t)

(import
  :gerbil/gambit/ports
  :std/format :std/misc/list :std/misc/process :std/srfi/13 :std/sugar :std/text/json
  :utils/base :utils/basic-parsers :utils/concurrency :utils/date :utils/filesystem
  :utils/generator :utils/json :utils/list :utils/number :utils/path-config :utils/version)

;;; Logging text to a series of log files.
(def (text-logger name: (name #f) on-new-file: (on-new-file #f))
  (sequentialize
   ['text-logger name]
   (let ((current-port #f)
         (new-file-hook (ensure-function on-new-file)))
     (λ (file text on-new-file: (on-new-file new-file-hook))
       (let* ((previous-port current-port)
              (previous-file (and previous-port (##port-name previous-port))))
         (unless (and previous-port (equal? previous-file file))
           (create-directory* (path-directory file))
           (set! current-port (open-output-file [path: file append: #t]))
           ;; Call the log transition hook, after the current-file has been switched,
           ;; but before it has been used, so the hook has a change to write a header.
           ((ensure-function on-new-file)
            previous-file: previous-file previous-port: previous-port
            current-file: file current-port: current-port)
           (when previous-port
             (close-output-port previous-port))))
       (display text current-port)
       (force-output current-port)
       (void)))))

;;; Logging JSON into a directory
(def (json-logger path top: (top (data-directory)) name: (name #f))
  (def directory (subpath top path))
  (def (text-line timestamp json)
    (call-with-output-string
      '() (λ (o) (display-timestamp timestamp o) (display " " o)
             (write-json json o) (newline o))))
  (def log (text-logger name: (or name path)))
  (def date-string<-timestamp (caching-date-string<-timestamp))
  (sequentialize
   ['json-logger (or name path)]
   (λ (json (timestamp (current-timestamp)))
     (let* ((text (text-line timestamp json))
            (date (date-string<-timestamp timestamp))
            (file (string-append directory "/" date ".log")))
       (log file text
            on-new-file:
            (λ (previous-file: _ previous-port: _ current-file: _ current-port: port)
              (display (text-line timestamp (metadata name: (or name path))) port)))))))

(def (json-data-logger . x)
  (json-logger (string-join x "/")))

;; Read from a log port a log entry as a cons of a timestamp and
;; (skipping leading whitespace) a string containing the rest of the line.
(def (read-log-entry port (decode identity))
  (cons (expect-timestamp port)
        (begin (expect-and-skip-any-whitespace port)
               (read-line port))))

(def (for-each-port-log-entry! port fun)
  (until (port-eof? port)
    (fun (read-log-entry port))
    (expect-and-skip-any-whitespace port)))

(def (read-all-log-entries port)
  (with-list-builder (c) (for-each-port-log-entry! port c)))

(def (read-file-log-entries path)
  (call-with-input-file path read-all-log-entries))

(def (read-xz-log-entries path)
  (run-process ["xz" "--decompress" "--stdout" path]
               coprocess: read-all-log-entries))

(def (for-each-xz-log-entry! path fun)
  (run-process ["xz" "--decompress" "--stdout" path]
               coprocess: (cut for-each-port-log-entry! <> fun)))

(def (for-each-xz-logdir-entry! file<-date-string start-timestamp end-timestamp fun)
  (def (good-timestamp? x)
    (and (<= start-timestamp x) (< x end-timestamp)))
  (def (partial-day-fun x)
    (when (good-timestamp? (car x)) (fun x)))
  (def (process-day date fun)
    (when-let ((file (file<-date-string (date-string<-timestamp date))))
      (for-each-xz-log-entry! file fun)))

  (def date ;; variable: the current date being process
    (period-start one-day start-timestamp)) ;; start at the start
  (def end-date (period-start one-day end-timestamp))

  (when (< start-timestamp end-timestamp)
    (unless (= date start-timestamp)
      (process-day date partial-day-fun)
      (increment! date one-day))
    (while (< date end-date)
      (process-day date fun)
      (increment! date one-day))
    (when (< date end-timestamp)
      (process-day date partial-day-fun))))

(def (generating<-xz-logdir file<-date-string start-timestamp end-timestamp)
  (generating-peeking<-coroutine
   (cut for-each-xz-logdir-entry! file<-date-string start-timestamp end-timestamp <>)))

;; Skip metadata lines, delayedly process json log line with processor function
(def (processing-log-object
      entry-processor
      object-decoder: (object-decoder identity)
      metadata-hook: (metadata-hook void))
  (λ-match
   ([timestamp . line]
    (if (metadata-line? line)
      (metadata-hook line)
      (entry-processor (cons timestamp (delay (object-decoder (<-json line)))))))))

(def (generating-peeking-processed-log-entries<-xz-logdir
      file<-date-string start-timestamp end-timestamp
      object-decoder: (object-decoder identity)
      metadata-hook: (metadata-hook void))
  (generating-peeking<-coroutine
   (λ (yield)
     (for-each-xz-logdir-entry!
      file<-date-string start-timestamp end-timestamp
      (processing-log-object yield object-decoder: object-decoder metadata-hook: metadata-hook)))))


;; We log a metadata entry as the first thing in every file.
;; This is a list and not a hash-table because we want it to be easy to match
;; by looking at a prefix. It could have been a two-entry list of "metadata" and a hash-table,
;; but at this point that would break backward compatibility for no advantage.
(def (metadata . keys)
  ["metadata"
   "software" software-name
   "version" software-version
   "machine" (machine-name)
   . keys])

;; Convert a metadata list to an array. Return #f if it was not metadata
(def (table<-metadata metadata)
  (match metadata
    (["metadata" "software" software "version" version "machine" machine . keys]
     (list->hash-table (cdr metadata)))
    (else #f)))

;; Recognize whether a string starts the JSON encodingn of a metadata list.
(def (metadata-line? line)
  (string-prefix? "[\"metadata\"," line))

;; Move this utility somewhere else? It's useful to force the promises from logs
(def force-pairs
  (λ-match
   ([a . b] (cons (force-pairs a) (force-pairs b)))
   (x (force x))))
