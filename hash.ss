;; -*- Gerbil -*-
;;;; hash-table utilities

;; NB: Most functions previously here are now in std/misc/hash, as of gerbil v0.16-DEV-536-geac7706d
;; Some have been renamed. See gerbil's doc/reference/misc.md

(export
  count-hash<-vector
  invert-hash<-vector/fold
  sum<-hash-values
  hash-keys/sort
  hash-table<-test
  hashset<-list
  hash-removed
  hash-filter!
  hash-filter-keys!
  hash-restrict-keys!)

(import
  :std/misc/hash :std/sort)

(def (count-hash<-vector
      from start: (start 0) end: (end (vector-length from)) to: (to (make-hash-table)))
  (invert-hash<-vector/fold from start: start end: end to: to nil: 0 cons: 1+))

(def (sum<-hash-values hash)
  (hash-fold (lambda (_ v acc) (+ v acc)) 0 hash))

(def (hash-keys/sort hash pred)
  (sort (hash-keys hash) pred))

(def (hash-table<-test test)
  (cond
   ((equal? test equal?) (make-hash-table))
   ((equal? test eqv?) (make-hash-table-eqv))
   ((equal? test eq?) (make-hash-table-eq))
   (else (error "Invalid equality predicate" test))))

(def (hashset<-list list (test equal?))
  (def h (hash-table<-test test))
  (for-each (cut hash-put! h <> #t) list)
  h)

(def (hash-removed hash key (default false))
  (let/cc return
    (begin0 (hash-ref/default hash key (lambda () (return (default))))
      (hash-remove! hash key))))

(def (hash-filter! hash pred)
  (hash-for-each (lambda (k v) (unless (pred k v) (hash-remove! hash k))) hash))

(def (hash-filter-keys! hash pred)
  (hash-filter! hash (lambda (k _) (pred k))))

;; Remove from a hash-table all the keys that are not among those specified (if any).
;; : (Table V K) <- (Table V K) (List K)
(def (hash-restrict-keys! hash list)
  (hash-filter-keys! hash (let (h (hashset<-list list)) (cut hash-key? h <>))))

;; Specify a hash-table by its equality predicate or an expression, wherein
;; the equality predicate is one of the keywords equal? eqv? eq?
;; signifying a hash-table with given predicate, () signifying an equal? hash-table,
;; or an expression, signifying itself.
(defrules specify-hash-table (equal? eqv? eq?)
  ((_ ()) (make-hash-table))
  ((_ equal?) (make-hash-table))
  ((_ eqv?) (make-eqv-hash-table))
  ((_ eq?) (make-eq-hash-table))
  ((_ expr) expr))
