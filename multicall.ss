;; -*- Gerbil -*-
;;;; Support for building a single multicall binary that has all the fricfrac functionality.

(export #t)

(import
  :std/format :std/generic :std/getopt :std/iter
  :std/misc/hash :std/misc/list :std/misc/list-builder :std/misc/number
  :std/sort :std/srfi/13 :std/stxutil :std/sugar
  (for-syntax :std/stxutil)
  ./base ./exit ./list ./shell ./source ./versioning)

(def current-program (make-parameter []))
(def entry-points (make-hash-table))

(def (current-program-string (program (current-program)))
  (string-join (reverse (flatten-pair-tree program)) " "))

(defgeneric getopt-spec
  (lambda (spec)
    (cond
     ((list? spec) spec)
     ((nat? spec) (for/collect ((i (in-iota spec 1))) (argument (number->string i))))
     ((not spec) [(rest-arguments "rest")])
     (else (error "Bad getopt-spec")))))

(defgeneric call-with-processed-command-line
  (lambda (processor command-line function)
    (cond
     ((getopt? processor)
      (call-with-getopt-parse processor (getopt-parse processor command-line) function))
     ((list? processor)
      (call-with-processed-command-line (apply getopt processor) command-line function)))))

(defstruct entry-point (name function help getopt) transparent: #t)

(def (gopt->positional-names gopt)
  (def names '())
  (def rest-name #f)
  (def argkey |std/getopt#!top-key|)
  (for-each (lambda (arg)
              (cond
               ((or (|std/getopt#!reqarg?| arg) (|std/getopt#!optarg?| arg))
                (push! (argkey arg) names))
               ((|std/getopt#!rest?| arg) (set! rest-name (argkey arg)))))
            (|std/getopt#!getopt-args| gopt))
  (values (reverse names) rest-name))

(def (getopt-parse->positional-arguments! gopt h)
  (defvalues (names rest-name) (gopt->positional-names gopt))
  (def (extract n) (begin0 (hash-get h n) (hash-remove! h n)))
  (def positional (map extract names))
  (def rest (when/list rest-name (extract rest-name)))
  (append positional rest))

(def (stringify<? x y)
  (string<? (stringify x) (stringify y)))

(def (getopt-parse->function-arguments gopt h)
  (def positionals (getopt-parse->positional-arguments! gopt h))
  (append positionals
          (foldr (lambda (kv l) (cons* (keywordify (car kv)) (cdr kv) l)) '()
                 (hash->list/sort h stringify<?))))

(def (call-with-getopt-parse gopt hash fun)
  (apply fun (getopt-parse->function-arguments gopt hash)))

(def (entry-points-getopt-spec (h entry-points))
  (for/collect (([name . e] (hash->list/sort h stringify<?)))
    (apply command name help: (entry-point-help e)
           (getopt-spec (entry-point-getopt e)))))

;; TODO: allow registering a getopt: structure and/or other command information,
;; so we can show detailed help and automatically parse arguments?
;; TODO: also allow a preprocess: function to further process the result of getopt (if specified)
;; or the raw arguments (if no getopt specified).
(def (register-entry-point function
                           id: (id #f) name: (name #f) help: (help #f)
                           getopt: (getopt #f))
  (let (name (symbolify (or name (string-filter easy-shell-character? (stringify id)))))
    (hash-put! entry-points name (make-entry-point name function help getopt))))

;; TODO: syntax to specify not just help, but getopt, etc.
(defrule (define-entry-point (id . formals) (options ...) body ...)
  (begin (def (id . formals) body ...)
         (register-entry-point id id: 'id options ...)))

(def multicall-default 'help)

(def (set-default-entry-point! x)
  (set! multicall-default x))

(define-entry-point (help (command #f))
  (help: "Print help about available commands"
   getopt: [(optional-argument 'command help: "subcommand for which to display help")])
  (awhen (id (software-identifier)) (displayln id))
  (def gopt (apply getopt (entry-points-getopt-spec)))
  (def program (current-program-string (cdr (current-program))))
  (if command
    (getopt-display-help-topic gopt (symbolify command) program)
    (getopt-display-help gopt program)))

(define-entry-point (meta)
  (help: "Print meta-information for completion purposes"
   getopt: [])
  (displayln (string-join (sort (map stringify (hash-keys entry-points)) string<?) " ")))

;; TODO: add a flag for short?
(define-entry-point (version complete: (complete #f) layer: (layer #f))
  (help: "Print software version"
   getopt: [(flag 'complete "-C" "--complete" help: "also show versions of previous layers")
            (option 'layer "-L" "--layer" help: "show versions for said layer")])
  (show-version complete: complete layer: layer))

(def (call-entry-point/internal command args)
  (match (hash-get entry-points (symbolify command))
    (#f (raise (format "Unknown command ~s. Try command help.\n" command)))
    ((entry-point _name fun _help getopt)
     (parameterize ((current-program (cons command (current-program))))
       (call-with-processed-command-line getopt args fun)))))

(def (call-entry-point . args)
   (with-abort-on-error ()
    (eval-print-exit
     (match args
       ([] (call-entry-point/internal multicall-default []))
       ([command . args] (call-entry-point/internal command args))))))

(defsyntax (define-multicall-main stx)
  (syntax-case stx ()
    ((_ ctx) (with-syntax ((main (identifierify #'ctx "main")))
               #'(begin (define main call-entry-point))))
    ((d) #'(d d))))
