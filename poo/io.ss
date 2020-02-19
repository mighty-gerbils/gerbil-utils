(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  (prefix-in (only-in :gerbil/gambit/bytes read-byte write-byte read-bytes write-bytes) port-)
  :std/sugar :std/iter
  :clan/utils/base :clan/utils/number
  :clan/poo/poo :clan/poo/mop :clan/poo/brace)

;; Byte <- In
(.defgeneric (read-byte in)
  (λ (in)
    (if (input-port? in) (port-read-byte in)
        (error "Trying to read-byte from unsupported object" in))))

;; Unit <- Out Byte
(.defgeneric (write-byte out byte))

;; Unit <- In Bytes ?offset: Nat ?length: Nat
(.defgeneric (read-bytes-into in bs offset: (offset 0) length: (length (- (bytes-length bs) offset)))
   (λ (in l)
    (for (i (in-range length))
      (let ((b (read-byte in))) ;; TODO: handle EOF, return number of bytes read???
        (bytes-set! bs (+ i offset) b)))))

;; Bytes <- In Nat
(.defgeneric (read-bytes in length)
  (λ (in length)
    (def bs (make-bytes length))
    (if (input-port? in)
      (let ((n (port-read-bytes bs in))) (assert! (= n length)))
      (read-bytes-into in bs length: length))
    bs))

;; Unit <- Out Bytes ?offset: Nat ?length: Nat
(.defgeneric (write-bytes out bs offset: (offset 0) length: (length (- (bytes-length bs) offset)))
   (λ (out bs offset: (offset 0) length: (length (- (bytes-length bs) offset)))
     (for (i (in-range length))
       (write-byte out (bytes-ref bs (+ i offset))))))

;; Nat <- In Nat
(def (read-integer-bytes in l)
  (nat<-bytes (read-bytes in l)))

;; Unit <- Out Int Nat
(def (write-integer-bytes out n l)
  (write-bytes out (bytes<-nat n l)))

;; Int <- In
(def (read-varint in)
  (let ((x (read-byte in)))
    (if (< x 128)
      (cond
       ((< x 64) x)
       ((< x 127) (let* ((l (- x 64))
                         (n (read-integer-bytes in l)))
                    (assert! (>= n 64))
                    (assert! (= (integer-length-in-bytes n) l))
                    n))
       (else ; (= x 127)
        (let* ((l (read-varint in))
               (n (read-integer-bytes in l)))
          (assert! (> l 62))
          (assert! (= (integer-length-in-bytes n) l))
          n)))
      (cond
       ((>= x 192) (- x 256))
       ((> x 128) (let* ((l (- 192 x))
                         (n (bitwise-ior (arithmetic-shift -1 l) (read-integer-bytes in l))))
                    (assert! (< n -64))
                    (assert! (= l (integer-length-in-bytes n)))
                    n))
       (else ; (= x 128)
        (let* ((l (read-varint in))
               (n (bitwise-ior (arithmetic-shift -1 l) (read-integer-bytes in l))))
          (assert! (> l 62))
          (assert! (= l (integer-length-in-bytes n)))
          n))))))

;; Unit <- Out Int
(def (write-varint out n)
  (if (negative? n)
    (if (>= n -64) (write-byte out (bitwise-and 255))
        (let ((l (integer-length-in-bytes n)))
          (if (<= 62)
            (begin
              (write-byte out (- 192 l))
              (write-integer-bytes out n l))
            (begin
              (write-byte out 128)
              (write-varint out l)
              (write-integer-bytes out n l)))))
    (if (<= n 63) (write-byte out n)
        (let ((l (integer-length-in-bytes n)))
          (if (<= l 62)
            (begin
              (write-byte out (+ l 64))
              (write-integer-bytes out n l))
            (begin
              (write-byte out 127)
              (write-varint out l)
              (write-integer-bytes out n l)))))))
