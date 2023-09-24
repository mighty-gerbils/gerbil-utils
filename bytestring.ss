;; Bytes as strings
(export #t)

(import
  :gerbil/gambit
  :std/error :std/iter :std/misc/list :std/srfi/43 :std/sugar)

(def (byte? x)
  (and (fixnum? x) (<= 0 x 255)))
(def (b x)
  (cond
   ((char? x) (char->integer x))
   ((string? x) (string->bytes x 'ISO-8859-1))
   ((u8vector? x) x)
   ((byte? x) x)
   (else (error "Not byteable" x))))
(def (c x)
  (cond
   ((byte? x) (integer->char x))
   ((u8vector? x) (bytes->string x 'ISO-8859-1))
   ((string? x) x)
   ((char? x) x)
   (else (error "Not charable" x))))

;;; constants
(def +bcr+ #u8(13))
(def +bcrlf+ #u8(13 10))
(def +blf+ #u8(10))
(def +bline-endings+ (vector +blf+ +bcr+ +bcrlf+))
(def +bstrict-line-endings+ (vector +bcrlf+ +blf+))

(def (u8vector-value-index val bs (start 0) (end (u8vector-length bs)))
  (declare (fixnum) (not safe))
  (check-argument (u8vector? bs) "u8vector" bs)
  (let/cc return
    (for (i (in-range start end))
      (when (= val (u8vector-ref bs i)) (return i)))
    #f))

(def buffer-length 65536)

(def (for-byte-lines port f)
  (declare (fixnum) (not safe))
  (let/cc return
    (def buf (make-u8vector buffer-length 0))
    (def other-bufs [])
    (def (newbuf)
      (set! buf (make-u8vector buffer-length 0)))
    (def start 0)
    (def end 0)
    (def (fill-buf)
      (set! start 0)
      (set! end (read-u8vector buf port)))
    (def (get-buf start end)
      (if (and (zero? start) (= end buffer-length))
        (begin0 buf (newbuf))
        (subu8vector buf start end)))
    (let loop ()
      (when (>= start end)
        (fill-buf)
        (when (zero? end)
          ;; Flush
          (unless (null? other-bufs)
            (f (apply u8vector-append (reverse other-bufs))))
          (return)))
      (cond
       ((u8vector-value-index 10 buf) =>
        (lambda (ix)
          (def b (get-buf start ix))
          (if (null? other-bufs)
            (f b)
            (begin
              (f (apply u8vector-append (reverse (cons b other-bufs))))
              (set! other-bufs '())))
          (set! end (1+ ix))))
       (else ;; Keep looking
        (push! (get-buf start end) other-bufs)
        (fill-buf)))
      (loop))))

(def (count-lines/u8vector u8vector start end)
  (declare (fixnum) (not safe))
  (for/fold (l 0) (i (in-range start end)) (if (= (u8vector-ref u8vector i) 10) (1+ l) l)))

(def (count-lines/port port)
  (declare (fixnum) (not safe))
  (def buf (make-u8vector buffer-length 0))
  (let loop ((i 0))
    (def c (read-u8vector buf port))
    (if (zero? c) i
        (loop (+ i (count-lines/u8vector buf 0 c))))))

(def (vector-member x vec)
  (vector-index (cut equal? x <>) vec))
