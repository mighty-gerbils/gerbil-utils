;; -*- Gerbil -*-
;;;; hash-table utilities

;; NB: Most functions previously here are now in std/misc/hash, as of gerbil v0.16-DEV-536-geac7706d
;; Some have been renamed. See gerbil's doc/reference/misc.md

(export #t)

(def (hash-key-value-map h kf vf)
  (def hf (make-hash-table))
  (hash-for-each (lambda (k v) (hash-put! hf (kf k) (vf v))) h)
  hf)
