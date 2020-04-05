;; -*- Gerbil -*-
;;;; General-purpose logging facility.
;; TODO: modify the interface to allow for actions on log rotation.

(export
  #t)

(import
  :gerbil/gambit/ports
  :std/format :std/misc/list :std/logger :std/misc/process :std/misc/sync
  :std/srfi/13 :std/sugar :std/text/json
  :clan/utils/base :clan/utils/basic-parsers :clan/utils/concurrency
  :clan/utils/date :clan/utils/filesystem
  :clan/utils/generator :clan/utils/json :clan/utils/list :clan/utils/memo :clan/utils/number
  :clan/utils/path :clan/utils/path-config :clan/utils/version)

;;; Logging text to a series of log files.
;; Start a new logger, with given name (optional) and a hook to call when switching files.
;; The logger itself is a function that takes two Strings and a hook:
;; the first string is a file name, the second string is text to log,
;; and the hook (which defaults to the hook passed to the logger)
;; is called whe the file name changed.
;; (<- String String) <- name: (Optional Any) on-new-file: (Optional FunctionDesignator)
(define-memo-function (text-logger name: (name #f) on-new-file: (on-new-file #f))
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

(def (log-line<-json timestamp json)
  (call-with-output-string
    '() (λ (o) (display-timestamp timestamp o) (display " " o)
           (write-json json o) (newline o))))


;;; Logging JSON into a directory
;; Start a new JSON logger, given a path of a subdirectory relative to an optional top: directory,
;; the latter which defaults to (data-directory), and a name that defaults to that path.
;; The logger itself is a function that takes an object that can be converted to JSON,
;; and an optional timestamp that defaults to (current-timestamp),
;; and logs a line containing the timestamp (as a 64-bit integer)
;; followed by the JSON text in a single line.
;; the first string is a file name, the second string is text to log,
;; and the hook (which defaults to the hook passed to the logger)
;; is called whe the file name changed.
;; (<- Any (Optional Timestamp)) <- String top: (Optional String) name: (Optional Any)
(define-memo-function ((json-logger
                        normalization: (λ (path top: (top (data-directory)) name: (name #f))
                                         [path top name]))
                       path top name)
  (def directory (subpath top path))
  (def log (text-logger name: (or name path)))
  (def date-string<-timestamp (caching-date-string<-timestamp))
  (sequentialize
   ['json-logger (or name path)]
   (λ (json (timestamp (current-timestamp)))
     (let* ((text (log-line<-json timestamp json))
            (date (date-string<-timestamp timestamp))
            (file (string-append directory "/" date ".log")))
       (log file text
            on-new-file:
            (λ (previous-file: _ previous-port: _ current-file: _ current-port: port)
              (display (log-line<-json timestamp (metadata name: (or name path))) port)))))))

;;; Logging JSON into a directory named after the arguments under the data-directory
;; (<- Any (Optional Timestamp)) <- String *
(def (json-data-logger . x)
  (json-logger (string-join x "/")))

;;; Logging JSON into a directory named after the arguments under the run-directory
;; (<- Any (Optional Timestamp)) <- String *
(def (json-run-logger . x)
  (json-logger (string-join x "/") top: (run-directory)))

;; Read from a log port a log entry as a cons of a timestamp and
;; (skipping leading whitespace) a string containing the rest of the line.
;; : (Pair Integer String) <- Port
(def (read-log-entry port)
  ;; TODO: gracefully handle bad input
  (cons (expect-timestamp port)
        (begin (expect-and-skip-any-whitespace port)
               (read-line port))))

;; Call a function on each entry in a log file
;; : <- Port (<- (Pair Integer String))
(def (for-each-port-log-entry! port fun)
  (until (port-eof? port)
    (fun (read-log-entry port))
    (expect-and-skip-any-whitespace port)))

;; Return the list of all entries in a log file port
;; : (List (Pair Integer String)) <- Port
(def (read-all-log-entries port)
  (with-list-builder (c) (for-each-port-log-entry! port c)))

;; Return the list of all entries in a named log file
;; : (List (Pair Integer String)) <- String
(def (read-file-log-entries path)
  (call-with-input-file path read-all-log-entries))

;; Return the list of all entries in a named log file compressed with xz
;; : (List (Pair Integer String)) <- String
(def (read-xz-log-entries path)
  (run-process ["xz" "--decompress" "--stdout" path]
               coprocess: read-all-log-entries))

;; Call a function on each entry in a named log file compressed with xz
;; : <- String (<- (Pair Integer String))
(def (for-each-xz-log-entry! path fun)
  (run-process ["xz" "--decompress" "--stdout" path]
               coprocess: (cut for-each-port-log-entry! <> fun)))

;; Call function fun on log each entry between start-timestamp (inclusive) and end-timestamp
;; (exclusive) given function file<-datestring that turns the timestamp for beginning of day (UTC)
;; into a filename for a log file compressed with xz.
;; : <- (String <- Timestamp) Timestamp Timestamp (<- (Pair Integer String))
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

;; Return two function values, a generator and a peeker, for all log entries between
;; start-timestamp (inclusive) and end-timestamp (exclusive) given function file<-datestring
;; that turns the timestamp for beginning of day (UTC)
;; into a filename for a log file compressed with xz.
;; : (Values (Generator (Pair Timestamp String)) (Peeker (Pair Timestamp String))) <- \
;;   (String <- Timestamp) Timestamp Timestamp
(def (generating<-xz-logdir file<-date-string start-timestamp end-timestamp)
  (in-cothread/peekable
   (cut for-each-xz-logdir-entry! file<-date-string start-timestamp end-timestamp <>)))

;; Given a entry-processor function to run on non-metadata entries,
;; a decoder function to transform entries from JSON into objects,
;; and a metadata hook to call on metadata lines,
;; return a function that given an log entry, calls the hook if it's a metadata line,
;; and calls the entry-processor with the pair of the timestamp and a delayed promise
;; to decode the rest of the line if it's a regular entry.
;; : X <- (X <- (Pair Timestamp (Promise E))) \
;;   object-decoder: (E <- String) metadata-hook: (X <- Timestamp (Promise (List String)))
;;   (String <- Timestamp) Timestamp Timestamp
(def (processing-log-object
      entry-processor
      object-decoder: (object-decoder identity)
      metadata-hook: (metadata-hook void))
  (λ-match
   ([timestamp . line]
    (let-syntax ((delay-warn
                  (syntax-rules ()
                    ((_ form) (delay (try form
                                          (catch (_)
                                            (warning "Bad log entry ~a ~a" timestamp line)
                                            (void))))))))
      (if (metadata-line? line)
        (metadata-hook timestamp (delay-warn (<-json line)))
        (entry-processor (cons timestamp (delay-warn (object-decoder (<-json line))))))))))

;; Generate log entries for compressed logs between two timestamps.
;; Given a function file<-datestring that turns the timestamp for beginning of day (UTC)
;; into a filename for a log file compressed with xz,
;; a start-timestamp (inclusive) and end-timestamp (exclusive),
;; an object decoder function taking JSON as input,
;; an optional metadata hook to process metadata lines,

;; Given a entry-processor function to run on non-metadata entries,
;; a decoder function to transform entries from JSON into objects,
;; and a metadata hook to call on metadata lines,
;; return a function that given an log entry, calls the hook if it's a metadata line,
;; and calls the entry-processor with the pair of the timestamp and a delayed promise
;; to decode the rest of the line if it's a regular entry.
;; : (Generator-Peeker-Shutter (Pair Timestamp (Promise E))) <- \
;;   (String <- Timestamp) Timestamp Timestamp \
;;   object-decoder: (E <- String) metadata-hook: (<- Timestamp (Promise (List String)))
(def (generating-peeking-processed-log-entries<-xz-logdir
      file<-date-string start-timestamp end-timestamp
      object-decoder: (object-decoder identity)
      metadata-hook: (metadata-hook void))
  (in-cothread/peekable
   (λ (yield)
     (for-each-xz-logdir-entry!
      file<-date-string start-timestamp end-timestamp
      (processing-log-object yield object-decoder: object-decoder metadata-hook: metadata-hook)))))


;; We log a metadata entry as the first thing in every file.
;; This is a list and not a hash-table because we want it to be easy to match
;; by looking at a prefix. It could have been a two-entry list of "metadata" and a hash-table,
;; but at this point that would break backward compatibility for no advantage.
;; : (List String) <- String *
(def (metadata . keys)
  ["metadata"
   "software" software-name
   "version" software-version
   "machine" (machine-name)
   . keys])

;; Convert a metadata list to an array. Return #f if it was not metadata
;; (Or (Table String String) '#f) <- (List String)
(def (table<-metadata metadata)
  (match metadata
    (["metadata" "software" software "version" version "machine" machine . keys]
     (list->hash-table (cdr metadata)))
    (else #f)))

;; Recognize whether a string starts the JSON encodingn of a metadata list.
;; Bool <- String
(def (metadata-line? line)
  (string-prefix? "[\"metadata\"," line))


;; Traverse a tree of pairs and force any promise in it.
;; Any <- Any
;; TODO: Move this utility somewhere else?
;; It's useful to force the promises from logs, but isn't specific to loggers.
(def force-pairs
  (λ-match
   ([a . b] (cons (force-pairs a) (force-pairs b)))
   (x (force x))))
