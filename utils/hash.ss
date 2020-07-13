;; -*- Gerbil -*-
;;;; hash-table utilities

;; NB: Most functions previously here are now in std/misc/hash, as of gerbil v0.16-DEV-536-geac7706d
;; Some have been renamed. See gerbil's doc/reference/misc.md

(export #t)

(import
  :std/iter)

(def (hash-key-value-map h kf vf)
  (def hf (make-hash-table))
  (hash-for-each (lambda (k v) (hash-put! hf (kf k) (vf v))) h)
  hf)

;; TODO: modify std/misc/hash#invert-hash<-vector
(def (invert-hash<-vector*
      from start: (start 0) end: (end (vector-length from))
      to: (to (make-hash-table)) key: (key identity))
  (for (i (in-range start end)) (hash-put! to (key (vector-ref from i)) i))
  to)

;; TODO: modify std/misc/hash#invert-hash<-vector/fold
(def (invert-hash<-vector/fold*
      from start: (start 0) end: (end (vector-length from))
      to: (to (make-hash-table)) nil: (nil '()) cons: (cons cons) key: (key identity))
  (for (i (in-range start end))
    (def val (vector-ref from i))
    (hash-put! to (key val) (cons i (hash-ref to val nil))))
  to)

(def (count-hash<-vector
      from start: (start 0) end: (end (vector-length from)) to: (to (make-hash-table)))
  (invert-hash<-vector/fold* from start: start end: end to: to nil: 0 cons: 1+))

(def (sum<-hash-values hash)
  (hash-fold (lambda (_ v acc) (+ v acc)) 0 hash))
