;; -*- Gerbil -*-
;;;; Utilities for files

(export
  #t)

(import
  :gerbil/gambit
  :std/format :std/misc/ports :std/sugar :std/pregexp
  ./base ./path ./ports ./temporary-files)

;; Atomically replace a file by one produced from the contents using output-contents
;; If salt? is true, also salt the previous contents of the file.
(def (clobber-file file contents settings: (settings '()) salt?: (salt? #f))
  (let* ((target (path-maybe-normalize file))
         (directory (path-directory target)))
    (call-with-temporary-file
     directory: directory
     prefix: (path-strip-directory target)
     settings: settings
     while-open: (λ (port path) (output-contents contents port))
     after-close: (λ (path)
                    (if salt?
                      (let ((port #f) (len #f))
                        (try
                         (set! port (ignore-errors
                                     (open-file [path: target create: #f
                                                       direction: 'input-output buffering: #f])))
                         (rename-file path target)
                         (when port
                           ;; TODO: is there a more portable and robust way to get the SEEK_END
                           ;; constant than hardcoding 2?
                           (set! len (input-port-byte-position port 0 2))
                           (output-port-byte-position port 0 0)
                           (write-u8vector (random-bytes len) port))
                         (finally (when (port? port) (close-port port)))))
                      (rename-file path target)))))) ;; should be atomic, at least on Unix

;; Run the contents of a file into a transformer, then,
;; if the new contents are different from the old contents,
;; atomically replace the file by one with the new contents.
(def (maybe-replace-file
      file transformer
      reader: (reader read-all-as-string)
      writer: (writer #f)
      comparator: (comparator equal?)
      settings: (settings '()))
  (printf "Transforming file ~a... " file)
  (let* ((old-contents (call-with-input-file [path: file . settings] reader))
         (new-contents (transformer old-contents)))
    (if (comparator old-contents new-contents)
      (printf "no changes needed!~%")
      (begin
        (clobber-file file (λ (port) ((or writer display) new-contents port)) settings: settings)
        (printf "done.~%"))))
  (void))
