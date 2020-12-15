;; -*- Gerbil -*-
;;;; Support for building a single multicall binary that has all the fricfrac functionality.

(export
  register-entry-point
  define-entry-point
  call-entry-point
  set-default-entry-point!)

(import
  :std/format :std/misc/list :std/sort :std/srfi/13 :std/sugar
  ./base ./list ./versioning ./exit)

(def entry-points (make-hash-table))

;; TODO: allow registering a getopt structure and/or other command information,
;; so we can show detailed help?
(def (register-entry-point name function help: (help #f))
  (hash-put! entry-points name [function help]))

;; TODO: syntax to specify not just help, but getopt, etc.
(defrules define-entry-point ()
  ((_ (id . formals) help . body)
   (begin (def (id . formals) . body)
          (register-entry-point (symbol->string 'id) id help: help))))

(def multicall-default "help")

(def (set-default-entry-point! x)
  (set! multicall-default x))

(define-entry-point (help)
  "Print help about available commands"
  (awhen (id (software-identifier)) (displayln id))
  (printf "commands: (default: ~a)\n" multicall-default)
  (def names (sort (hash-keys entry-points) string<))
  (def longest-name-length (extremum<-list > (map string-length names)))
  (for-each! names
             (λ (name)
               (printf "~a   ~a\n"
                       (string-pad-right name longest-name-length #\space)
                       (cadr (hash-get entry-points name))))))

(define-entry-point (meta)
  "Print meta-information for completion purposes"
  (displayln (string-join (sort (hash-keys entry-points) string<?) " ")))

;; TODO: add a flag for short?
(define-entry-point (version)
  "Print software version"
  (show-version complete: #t))

(def (call-entry-point . args)
  (with-exit-on-error ()
    (eval-print-exit
     (match args
       ([] ((car (hash-get entry-points multicall-default))))
       ([command . args]
        (match (hash-get entry-points command)
          ('#f (raise (format "Unknown command ~s. Try command help.\n" command)))
          ([fun . _] (apply fun args))))))))
