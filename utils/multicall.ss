;; -*- Gerbil -*-
;;;; Support for building a single multicall binary that has all the fricfrac functionality.

(export
  register-entry-point
  define-entry-point
  call-entry-point)

(import
  :std/format :std/misc/list :std/sort :std/srfi/13
  ./base ./list ./version)

(def entry-points (make-hash-table))

(def (register-entry-point name function help: (help #f))
  (hash-put! entry-points name [function help]))

(defrules define-entry-point ()
  ((_ (id . formals) help . body)
   (begin (def (id . formals) . body)
          (register-entry-point (symbol->string 'id) id help: help))))

(def (multicall-help)
  (let ((id (software-identifier)))
    (when id (printf "~a\n" id)))
  (printf "commands:\n")
  (nest
   (let* ((names (sort (hash-keys entry-points) string<))
          (longest-name-length (extremum<-list > (map string-length names)))))
   (for-each! names) (λ (name))
   (printf "~a   ~a\n"
           (string-pad-right name longest-name-length #\space)
           (list-ref (hash-get entry-points name) 1))))

(register-entry-point "version" show-version help: "Print software version")
(register-entry-point "help" multicall-help help: "Print help about available commands")

(def (call-entry-point . args)
  (match args
    ([] (multicall-help))
    ([command . args]
     (match (hash-get entry-points command)
       ('#f (eprintf "Unknown command ~s. Try command help.\n" command)
            (exit 2))
       ([fun . _] (apply fun args))))))
