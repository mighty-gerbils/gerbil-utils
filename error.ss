;; -*- Gerbil -*-
;;;; General-purpose logging facility.
;; TODO: modify the interface to allow for actions on log rotation.

(export (except-out #t errorf warnf infof debugf verbosef))

(import
  :gerbil/gambit/os
  :std/error :std/format :std/logger
  ./base)

(deflogger clan)

(def (warn-and-err format type . args)
  (apply warnf format type args)
  (apply error type args))

(def (abort! code msg . args)
  (apply eprintf msg args)
  (exit code))

(def current-error-context (make-parameter '()))
