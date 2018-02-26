;; -*- Gerbil -*-
;;;; Utilities pertaining to using Vectors

(export #t)

(import
  :std/iter :std/misc/list :std/sugar
  :clan/utils/base :clan/utils/list :clan/utils/number)

;;; Assuming a sorted vector, a predicate on vector elements that is "increasing",
;;; i.e. if true, true on all subsequent elements, and optionally
;;; a start (inclusive, defaults to 0) and an end (exclusive, defaults to vector length),
;;; return the least index of a vector element in the interval [start, env)
;;; that satisfies the predicate, or the end if none does.
(def (least-vector-index pred? vector (start 0) (end (vector-length vector)))
  (least-integer (λ (i) (pred? (vector-ref vector i))) start end))

;;; Assuming a sorted vector, a predicate on vector elements that is "decreasing",
;;; i.e. if true, true on all preceding elements, and optionally
;;; a start (inclusive, defaults to 0) and an end (exclusive, defaults to vector length),
;;; return the most index such that all previous vector elements in the interval [start, env)
;;; satisfy the predicate, or the start if none does.
(def (most-vector-index pred? vector (start 0) (end (vector-length vector)))
  (least-integer (λ (i) (not (pred? (vector-ref vector i)))) start end))

;;; Copy a vector if necessary: return the same if no change in start and end requested.
(def (maybe-subvector vector (start 0) (end #f))
  (let* ((len (vector-length vector))
         (end (or end len)))
    (if (and (eqv? start 0) (eqv? end len))
      vector
      (subvector vector start end))))

(def (vector-for-each! vector function start: (start 0) end: (end #f))
  (for ((i (in-range start (- (or end (vector-length vector)) start))))
    (function (vector-ref vector i))))

(def (vector-for-each-indexed! vector function start: (start 0) end: (end #f))
  (for ((i (in-range start (- (or end (vector-length vector)) start))))
    (function i (vector-ref vector i))))

(def (vector-reverse-for-each! vector function start: (start 0) end: (end #f))
  (let ((end (or end (vector-length vector))))
    (for ((i (in-range (- end 1) (- end start) -1)))
      (function (vector-ref vector i)))))

(def (vector-reverse-for-each-indexed! vector function start: (start 0) end: (end #f))
  (let ((i (- (or end (vector-length vector)) 1)))
    (while (>= i start)
      (function i (vector-ref vector i)))))

(def (list<-vector vector start: (start 0) end: (end #f))
  (with-list-builder (c!) (vector-for-each! vector c! start: start end: end)))

(def vector<-cons (λ-match ([car . cdr] (vector car cdr)) (_ #f)))

;;;; Given a vector, an index and a function, update the element of the vector at given index
;;;; by invoking the function on its previous value
;;(def (vector-update! vector index fun)
;;  (vector-set! vector index (fun (vector-ref vector index))))
