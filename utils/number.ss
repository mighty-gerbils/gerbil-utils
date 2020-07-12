;; -*- Gerbil -*-
;;;; Utilities pertaining to using Numbers

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact
  :scheme/base
  :std/misc/bytes :std/sugar
  ./base)

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

(def (nat? n)
  (and (exact-integer? n) (not (negative? n))))

(def (fxnat? n)
  (and (fixnum? n) (not (negative? n))))

(def (nat-under? n) (λ (x) (and (nat? x) (< x n))))

(def (n-bytes<-n-bits n-bits) (arithmetic-shift (+ n-bits 7) -3))

(def (integer-length-in-bytes n) (n-bytes<-n-bits (integer-length n)))

(def (bytes<-nat n (n-bytes (integer-length-in-bytes n)))
  (def bytes (make-bytes n-bytes))
  (u8vector-uint-set! bytes 0 n big n-bytes)
  bytes)

(def (nat<-bytes bytes)
  (u8vector-uint-ref bytes 0 big (bytes-length bytes)))

;; Iterate a function with an integer argument ranging from one value
;; increasing by one until it reaches another value (excluded)
;; : <- Integer Integer (<- Integer)
(def (for-each-integer! from below fun)
  (let loop ((i from))
    (when (< i below)
      (fun i)
      (loop (+ i 1)))))

(defrules increment! ()
  ((_ place) (increment! place 1))
  ((_ place increment ...) (set! place (+ place increment ...))))
(defrules pre-increment! ()
  ((_ place increment ...) (begin (increment! place increment ...) place)))
(defrules post-increment! ()
  ((_ place increment ...) (begin0 place (increment! place increment ...))))

(defrules decrement! ()
  ((_ place) (decrement! place 1))
  ((_ place decrement ...) (set! place (- place decrement ...))))
(defrules pre-decrement! ()
  ((_ place decrement ...) (begin (decrement! place decrement ...) place)))
(defrules post-decrement! ()
  ((_ place decrement ...) (begin0 place (decrement! place decrement ...))))

(def (make-counter (n 0))
  (λ () (post-increment! n)))

;;; Assuming pred? is "increasing", i.e. if true for some integer, true for all larger integers,
;;; find the smallest integer in interval [start, end) for which pred? holds.
;;; If no integer in the interval satisfies pred?, return end. If all do, return start.
(def (least-integer pred? start end)
  (if (<= end start) end ; empty interval, return end.
      (let ((mid (arithmetic-shift (+ end start) -1))) ;; NB: happily we have bignums, so no overflow
        (if (pred? mid)
          (least-integer pred? start mid)
          (least-integer pred? (1+ mid) end)))))

;;; Assuming pred? is "decreasing", i.e. if true for some integer, true for all smaller integers,
;;; find the greatest integer in interval (start, end] for which pred? holds.
;;; If no integer in the interval satisfies pred?, return start. If all do, return end.
(def (most-integer pred? start end)
  (if (<= end start) start ; empty interval, return start.
      (let ((mid (arithmetic-shift (+ end start 1) -1))) ;; round up, trust bignums for no overflow
        (if (pred? mid)
          (most-integer pred? mid end)
          (most-integer pred? start (- 1 mid))))))


;;;; Comparison in R + infinities
;;;; #f is +infinity, #t is -infinity
(def (R< x y)
  (cond
   ((eq? x #t) (not (eq? y #t)))
   ((eq? x #f) #f)
   ((eq? y #t) #f)
   ((eq? y #f) #t)
   (else (< x y))))

(def (R<= x y) (not (R< y x)))

(def (display-integer/fit n width out)
  (assert! (exact-integer? n))
  (assert! (nat? n))
  (assert! (plus? width))
  (let* ((digits (number->string n))
         (padding (- width (string-length digits))))
    (assert! (nat? padding))
    (display (make-string padding #\0) out)
    (display digits out)))

;; Convert one digit to a roman numeral, given strings for one unit, five units and ten units.
(def (roman-numeral<-digit digit (i "I") (v "V") (x "X"))
  (case digit
    ((0) "")
    ((1) i)
    ((2) (string-append i i))
    ((3) (string-append i i i))
    ((4) (string-append i v))
    ((5) v)
    ((6) (string-append v i))
    ((7) (string-append v i i))
    ((8) (string-append v i i i))
    ((9) (string-append i x))
    (else (error "incorrect digit" digit))))

(def (roman-numeral<-integer n)
  ;; NB: Only works for integer from 1 to 3999.
  (when (or (not (exact-integer? n)) (< n 1) (> n 3999))
    (error "I cannot convert ~s to a roman numeral" n))
  (let* ((units (modulo n 10))
         (n/10 (/ (- n units) 10))
         (tens (modulo n/10 10))
         (n/100 (/ (- n/10 tens) 10))
         (hundreds (modulo n/100 10))
         (thousands (/ (- n/100 hundreds) 10)))
    (string-append
     (roman-numeral<-digit thousands "M" "" "")
     (roman-numeral<-digit hundreds "C" "D" "M")
     (roman-numeral<-digit tens "X" "L" "C")
     (roman-numeral<-digit units "I" "V" "X"))))

(def (round/ x y)
  (cond
   ((zero? y) (/ x 0))
   ((plus? x)
    (if (plus? y)
      (let-values (((q r) (truncate/ (+ x (/ y 2)) y)))
        (values q (- r (/ y 2))))
      (let-values (((q r) (round/ x (- y))))
        (values (- q) r))))
    ((minus? x)
     (if (plus? y)
       (let-values (((q r) (round/ (- x) y)))
         (values (- q) (- r)))
       (let-values (((q r) (round/ (- x) (- y))))
         (values q (- r)))))
   ((zero? x) (values 0 0))))
