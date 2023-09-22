;; -*- Gerbil -*-
;;;; Utilities to manipulate temporary files

(export
  current-temporary-directory
  call-with-temporary-file
  make-temporary-file ;; not ready yet
  ;; make-temporary-file-template ;; internal only?
  ;; file-exists-exception? ;; move somewhere else?
  delete-file-if-exists ;; move somewhere else?
  )

(import
  :gerbil/gambit
  :std/error
  :std/sugar
  ;; :std/os/error ; for commented out make-temporary-file
  ./base ./random)

;; TODO: upstream as std/os/temp.ss

(def current-temporary-directory (make-parameter (getenv "TMPDIR" "/tmp")))

(def file-exists-exception-code #f)

(def (file-exists-exception? e)
  (and (os-exception? e)
       (or (and file-exists-exception-code
                (eqv? file-exists-exception-code (os-exception-code e)))
           (and (equal? "File exists" (##os-err-code->string (os-exception-code e)))
                (begin (set! file-exists-exception-code (os-exception-code e)) #f)))))

(def (make-temporary-file-template
      directory: (directory (current-temporary-directory))
      prefix: (prefix "tmp")
      random: (random "XXXXXX")
      suffix: (suffix ""))
  (call-with-output-u8vector
   []
   (λ (output-port)
     (display-temporary-file-template
      output-port
      directory: directory
      prefix: prefix
      random: random
      suffix: suffix))))

(def (display-temporary-file-template
      output-port
      directory: (directory (current-temporary-directory))
      prefix: (prefix "tmp")
      random: (random "XXXXXX")
      suffix: (suffix ""))
  (when directory
    (display directory output-port)
    (display "/" output-port))
  (when prefix
    (display prefix output-port))
  (display random output-port)
  (when suffix
    (display suffix output-port)))

(def (open-file-for-direction direction)
  (case direction
    ((io) open-file)
    ((input) open-input-file)
    ((output) open-output-file)
    (else (error "Invalid direction" direction))))

;; Create a temporary file, the pathname of which will be based on concatenating:
;; 1- the specified directory (unless it's #f, defaults to (current-temporary-directory))
;; 2- the prefix (unless it's #f, defaults to "tmp")
;; 3- XXXXXX (six X'es, that will be replaced by a string of alphanumerics by mkostemps)
;; 4- the suffix (unless it's #f, defaults to ".tmp")
;; Open the file using the direction: can be 'io, 'input or 'output
;; The settings: will be passed to open-file, open-input-file or output-file, respectively
;;
;; TODO: On supported platform (= Linux), use mkostemps instead of the below.
(def (make-temporary-file
      direction: (direction 'io)
      directory: (directory (current-temporary-directory))
      prefix: (prefix "tmp")
      suffix: (suffix "")
      settings: (settings '()))
  (let ((open (open-file-for-direction direction))
        (path #f)
        (port #f))
     (let loop ()
       (try
        (set! path (call-with-output-string
                     []
                     (λ (output-port)
                       (display-temporary-file-template
                        output-port
                        directory: directory
                        prefix: prefix
                        random: (six-alphanumerics)
                        suffix: suffix))))
        (set! port (open (append [path: path create: #t] settings)))
        (catch file-exists-exception? => (λ (e) (loop)))))
     (values port path)))

;; Call a THUNK with stream and/or pathname arguments identifying a temporary file.
;; 1- make a temporary file as per make-temporary-file,
;;  using arguments directory: prefix: suffix: settings:
;; 2- consume the temporary file as per use-temporary-file,
;;  using arguments while-open: after-close: keep:
;;
;; XXX vyzo: api issue
;;           where is the thunk? it seems to be the while-open argument
;;           this should be a required parameter for the function for compatibility
;;           with other call-with-* forms.
(def (call-with-temporary-file
      direction: (direction 'io)
      directory: (directory (current-temporary-directory))
      prefix: (prefix "tmp")
      suffix: (suffix ".tmp")
      settings: (settings '())
      while-open: (while-open #f)
      after-close: (after-close #f)
      keep: (keep? #f))
  ;; TODO: if and when we ever care about asynchronous interrupts,
  ;; bind port and path *atomically* into this function as continuation,
  ;; so there is always a try-finally after the bind?
  (let-values (((port path)
                (make-temporary-file
                 direction: direction
                 directory: directory
                 prefix: prefix
                 suffix: suffix
                 settings: settings)))
    (use-temporary-file
     port path
     while-open: while-open
     after-close: after-close
     keep: keep?)))

;; Try deleting a file, ignore error if the file didn't exist,
;; e.g. if it was already deleted or never created.
(def (delete-file-if-exists path)
  (try
   (delete-file path)
   (catch (no-such-file-or-directory-exception? e) (void))))

;; What to do with a temporary file after you get it somehow.
;; The while-open: thunk (unless #f) will be called with two arguments, stream and pathname.
;; The after-close: thunk (unless #f) will be called with one argument, pathname.
;; Finally, the file will be deleted, unless the keep: argument is true.
;;
;; XXX vyzo - api issue
;;     the thunk should be a required argument; it's meaningless to use a temporary
;;     file without consuming it somehow.
(def (use-temporary-file
      port path
      while-open: (while-open #f)
      after-close: (after-close #f)
      keep: (keep? #f))
  (try
   (let (result (void))
     (when while-open
       (set! result (while-open port path)))
     (close-port port) ;; unhappily, there is no way to atomically clear a flag after that.
     (when after-close
       (set! result (after-close path)))
     result)
   (finally
    (close-port port) ;; happily, close-port is idempotent.
    (when (not keep?)
      (delete-file-if-exists path)))))


;;; Below: incomplete attempt at using mkostemps, on Linux
;; TODO: either complete this Linux-specific code that uses mkostemps, or delete it.
;; TODO: share those FFI macros somehow?
#|
(extern _mkostemps) ;; NB: glibc-ism

(begin-foreign
  (c-declare #<<END-C
#define __USE_GNU 1
#include <stdlib.h>
#include <errno.h>
END-C
  )

  (define-macro (define-c-lambda id args ret #!optional (name #f))
    (let ((name (or name (##symbol->string id))))
      `(define ,id
         (c-lambda ,args ,ret ,name))))

  (define-macro (define-with-errno symbol ffi-symbol args)
    `(define (,symbol ,@args)
       (declare (not interrupts-enabled))
       (let ((r (,ffi-symbol ,@args)))
         (if (##fx< r 0)
           (##fx- (__errno))
           r))))

  (namespace ("clan/temporary-files#"
              __mkostemps _mkostemps
              __errno))

  (define-c-lambda __errno () int
    "___return (errno);")

  (define-c-lambda __mkostemps (scheme-object int int) int
    "ffi_mkostemps")

  (define-with-errno _mkostemps __mkostemps (template suffixlen flags))

  (c-declare "static int ffi_mkostemps (___SCMOBJ template_bytes, int suffixlen, int flags);")

  (c-declare #<<END-C
#define U8_DATA(obj) ___CAST (___U8*, ___BODY_AS (obj, ___tSUBTYPED))

int ffi_mkostemps (___SCMOBJ template_bytes, int suffixlen, int flags)
{
 return mkostemps (U8_DATA (template_bytes), suffixlen, flags);
}
END-C
))

(def (make-temporary-file
      directory: (directory (current-temporary-directory))
      prefix: (prefix "tmp")
      suffix: (suffix "")
      settings: (settings '()))
  (let* ((template (make-temporary-file-template
                    directory: directory
                    prefix: prefix
                    suffix: suffix))
         (suffixlen (u8vector-length (string->bytes suffix)))
         (flags 0) ;; TODO: deduce flags from settings
         (fd (check-os-error (_mkostemps template suffixlen flags)
                (make-temporary-file template suffixlen flags)))
         (name (bytes->string template)))
    (values (##open-predefined 'io name fd settings)
            name)))
|# ;|
