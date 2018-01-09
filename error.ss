;; -*- Gerbil -*-
;;;; General-purpose logging facility.
;; TODO: modify the interface to allow for actions on log rotation.

(export #t)

(import
  :gerbil/gambit/os
  :std/error :std/format :std/logger
  :utils/base)

(def (warn-and-err format type . args)
  (apply warning format type args)
  (apply error type args))

(def (abort! code msg . args)
  (apply eprintf msg args)
  (exit code))

