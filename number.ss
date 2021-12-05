;; -*- Gerbil -*-
;;;; Utilities pertaining to using Numbers

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact
  :scheme/base
  :std/assert :std/misc/bytes :std/sugar
  ./base ./ports)

;;;; Numbers
(def (integer-part real)
  (cond
   ((exact-integer? real) real)
   ((real? real) (inexact->exact (truncate real)))
   (else (error "Bad real" real))))

(def (fractional-part real)
  (cond
   ((exact-integer? real) 0)
   ((real? real) (- real (integer-part real)))
   (else (error "Bad real" real))))

(def (floor-align n alignment)
  (- n (modulo n alignment)))

(def (ceiling-align n alignment)
  (let ((mod (modulo n alignment)))
    (if (zero? mod) n (- (+ n alignment) mod))))

(def (plus? x) (< 0 x))
(def (minus? x) (> 0 x))
(def (sign<-real x)
  (cond ((< 0 x) +1) ((> 0 x) -1) (else 0)))

(def (nat? n)
  (and (exact-integer? n) (not (negative? n))))

(def (fxnat? n)
  (and (fixnum? n) (not (negative? n))))

(def (nat-under? n) (λ (x) (and (nat? x) (< x n))))

(def (n-bytes<-n-bits n-bits) (arithmetic-shift (+ n-bits 7) -3))

(def (integer-length-in-bytes n) (n-bytes<-n-bits (integer-length n)))

(def (bytes<-nat n (n-bytes (integer-length-in-bytes n)))
  (def bytes (make-bytes n-bytes 0))
  (when (positive? n-bytes)
    (u8vector-uint-set! bytes 0 n big n-bytes))
  bytes)

(def (nat<-bytes bytes)
  (def n-bytes (bytes-length bytes))
  (if (zero? n-bytes) 0 (u8vector-uint-ref bytes 0 big n-bytes)))

;; Iterate a function with an integer argument ranging from one value
;; increasing by one until it reaches another value (excluded)
;; : <- Integer Integer (<- Integer)
(def (for-each-integer! from below fun)
  (let loop ((i from))
    (when (< i below)
      (fun i)
      (loop (+ i 1)))))

;;; Binary search in interval [start, end) to find the least integer for which pred? holds,
;;; assuming pred? is "increasing", i.e. if true for some integer, true for all larger integers.
;;; If no integer in the interval satisfies pred?, return end. If all do, return start.
(def (least-integer pred? start end)
  (if (<= end start) end ; empty interval, return end.
      (let ((mid (arithmetic-shift (+ end start) -1))) ;; NB: happily we have bignums, so no overflow
        (if (pred? mid)
          (least-integer pred? start mid)
          (least-integer pred? (1+ mid) end)))))

;;; Binary search in interval (start, end] to find the most integer i for which pred? holds
;;; for all indexes in [start i), assuming pred? is "decreasing",
;;; i.e. if true for some integer, true for all smaller integers.
;;; If no integer in the interval satisfies pred?, return start. If all do, return end.
(def (most-integer pred? start end)
  (if (<= end start) start ; empty interval, return start.
      (let ((mid (arithmetic-shift (+ end start 1) -1))) ;; round up, trust bignums for no overflow
        (if (pred? mid)
          (most-integer pred? mid end)
          (most-integer pred? start (- 1 mid))))))

;;;; Comparison in R + infinities
;;;; #f is +infinity, #t is -infinity... or, just use +inf.0 and -inf.0 !!!
(def (R< x y)
  (cond
   ((eq? x #t) (not (eq? y #t)))
   ((eq? x #f) #f)
   ((eq? y #t) #f)
   ((eq? y #f) #t)
   (else (< x y))))

(def (R<= x y) (not (R< y x)))

(def (display-integer/fit n width out)
  (with-output (out)
    (assert! (exact-integer? n))
    (assert! (nat? n))
    (assert! (plus? width))
    (let* ((digits (number->string n))
           (padding (- width (string-length digits))))
      (assert! (nat? padding))
      (display (make-string padding #\0) out)
      (display digits out))))

;; TODO: make it so we round towards the closest even number when right in the middle
(def (round/ x y)
  (cond
   ((zero? y) (/ x 0))
   ((plus? x)
    (if (plus? y)
      (let (hy (arithmetic-shift y -1))
        (let-values (((q r) (truncate/ (+ x hy) y)))
          (values q (- r hy))))
      (let-values (((q r) (round/ x (- y))))
        (values (- q) r))))
   ((minus? x)
    (if (plus? y)
      (let-values (((q r) (round/ (- x) y)))
        (values (- q) (- r)))
      (let-values (((q r) (round/ (- x) (- y))))
        (values q (- r)))))
   ((zero? x) (values 0 0))))

;; TODO: accelerate that!
(def (expt-mod x e n)
  (modulo (expt x e) n))
