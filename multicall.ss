;; -*- Gerbil -*-
;;;; Support for building a single multicall binary that has all the fricfrac functionality.

(export
  register-entry-point
  define-entry-point
  call-entry-point
  set-default-entry-point!)

(import
  :std/format :std/misc/list :std/sort :std/srfi/13
  ./base ./list ./versioning ./exit)

(def entry-points (make-hash-table))

(def (register-entry-point name function help: (help #f))
  (hash-put! entry-points name [function help]))

(defrules define-entry-point ()
  ((_ (id . formals) help . body)
   (begin (def (id . formals) . body)
          (register-entry-point (symbol->string 'id) id help: help))))

(def multicall-default "help")

(def (set-default-entry-point! x)
  (set! multicall-default x))

(def (multicall-help)
  (let ((id (software-identifier)))
    (when id (printf "~a\n" id)))
  (printf "commands: (default: ~a)\n" multicall-default)
  (nest
   (let* ((names (sort (hash-keys entry-points) string<))
          (longest-name-length (extremum<-list > (map string-length names)))))
   (for-each! names) (λ (name))
   (printf "~a   ~a\n"
           (string-pad-right name longest-name-length #\space)
           (cadr (hash-get entry-points name)))))

(def (multicall-meta)
  (displayln (string-join (sort (hash-keys entry-points) string<?) " ")))

(register-entry-point "version" (lambda () (show-version complete: #t)) help: "Print software version")
(register-entry-point "help" multicall-help help: "Print help about available commands")
(register-entry-point "meta" multicall-meta help: "Print meta-information for completion purposes")

(def (call-entry-point . args)
  (eval-print-exit
   (match args
     ([] ((car (hash-get entry-points multicall-default))))
     ([command . args]
      (match (hash-get entry-points command)
        ('#f (eprintf "Unknown command ~s. Try command help.\n" command)
             (exit 2))
        ([fun . _] (apply fun args)))))))
