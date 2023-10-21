;; -*- Gerbil -*-
;;;; General-purpose logging facility.
;; TODO: modify the interface to allow for actions on log rotation.

(export #t)

(import
  (only-in :std/format eprintf)
  (only-in ./exception string<-exception))

(def (warn-and-err format type . args)
  (apply eprintlnf format type args)
  (apply error type args))

(def (abort! code fmt . args)
  (apply eprintlnf fmt args)
  (exit code))

(def current-error-context (make-parameter '()))

(def (eprintlnf fmt . args)
  (apply eprintf (string-append fmt "\n") args))

(def (log-error what exn)
  (eprintlnf "~a: ~a" what (string<-exception exn)))
