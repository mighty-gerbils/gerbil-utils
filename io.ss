(export #t)
(import
  (only-in :std/assert assert!)
  (only-in :std/misc/bytes big uint->u8vector u8vector->uint
           u8vector-u16-set!)
  (only-in :std/misc/number uint-length-in-u8))

;;(def (write-u8vector v p) (write-subu8vector v 0 (u8vector-length v) p))
;;(def (read-u8vector v p) (def l (u8vector-length v)) (read-subu8vector v 0 l p l))

;; : UInt16 <- In
(def (unmarshal-uint16 port) ;; big endian
  (def hi (read-u8 port))
  (def lo (read-u8 port))
  (fx+ lo (fxarithmetic-shift hi 8)))

;; : Bytes <- In
(def (unmarshal-sized16-u8vector port)
  (def size (unmarshal-uint16 port))
  (unmarshal-n-u8 size port))

;; Read a given number of bytes, even if the number is zero
;; : Bytes <- UInt In
(def (unmarshal-n-u8 size port)
  (if (zero? size)
    #u8()
    (let ((bs (make-u8vector size)))
      (assert! (= size (read-u8vector bs port)))
      bs)))

;; : <- UInt16 Out
(def (marshal-uint16 n port)
  (assert! (<= 0 n 65535))
  (def u8vector (make-u8vector 2 0))
  (u8vector-u16-set! u8vector 0 n big)
  (write-u8vector u8vector port))

;; : <- U8vector Out
(def (marshal-sized16-u8vector u8vector port)
  (marshal-uint16 (u8vector-length u8vector) port)
  (write-u8vector u8vector port))

;; : (U8vector <- 'a) <- (<- 'a Out)
(def (u8vector<-<-marshal marshal)
  (lambda (x) (call-with-output-u8vector (lambda (port) (marshal x port)))))

;; : ('a <- U8vector) <- ('a <- In)
(def (<-u8vector<-unmarshal unmarshal)
  (lambda (u8vector)
    (call-with-input-u8vector u8vector
     (lambda (port)
       (let ((v (unmarshal port)))
         (assert! (not (eof-object? v)))
         (assert! (eof-object? (read-u8 port)))
         v)))))

;; : (<- 'a Out) <- (U8vector <- 'a)
(def (marshal<-u8vector<- u8vector<-)
  (lambda (x port) (write-u8vector (u8vector<- x) port)))

;; : ('a <- In) <- ('a <- U8vector) UInt
(def (unmarshal<-<-u8vector <-u8vector n)
  (lambda (port) (<-u8vector (unmarshal-n-u8 n port))))

;; : UInt <- UInt In
(def (read-uint-u8vector length-in-u8 in)
  (u8vector->uint (unmarshal-n-u8 length-in-u8 in)))

;; : <- Int UInt+ Out
(def (write-uint-u8vector x length-in-u8 out)
  (write-u8vector (uint->u8vector x big length-in-u8) out))

;; Encoding and decoding integers into self-delimited byte streams, preserving lexicographic order
;; supposing the first byte is compared signed and the rest unsigned.
;; : Int <- In
(def (read-varint in)
  (let ((x (read-u8 in)))
    (if (< x 128)
      (cond
       ((< x 64) x)
       ((< x 127) (let* ((l (- x 63))
                         (n (read-uint-u8vector l in)))
                    (assert! (>= n 64))
                    (assert! (= (uint-length-in-u8 n) l))
                    n))
       (else ; (= x 127)
        (let* ((l (read-varint in))
               (n (read-uint-u8vector l in)))
          (assert! (>= l 64))
          (assert! (= (uint-length-in-u8 n) l))
          n)))
      (cond
       ((>= x 192) (- x 256))
       ((> x 128) (let* ((l (- 192 x))
                         (n (bitwise-ior (arithmetic-shift -1 l) (read-uint-u8vector l in))))
                    (assert! (< n -64))
                    (assert! (= l (uint-length-in-u8 n)))
                    n))
       (else ; (= x 128)
        (let* ((l (- (read-varint in)))
               (n (bitwise-ior (arithmetic-shift -1 l) (read-uint-u8vector l in))))
          (assert! (> l 63))
          (assert! (= l (uint-length-in-u8 n)))
          n))))))

;; : <- Int Out
(def (write-varint n out)
  (if (negative? n)
    (if (>= n -64) (write-u8 (bitwise-and 255) out)
        (let ((l (uint-length-in-u8 n)))
          (if (<= 63)
            (begin
              (write-u8 (- 192 l) out)
              (write-uint-u8vector n l out))
            (begin
              (write-u8 128 out)
              (write-varint (- l) out)
              (write-uint-u8vector n l out)))))
    (if (<= n 63) (write-u8 n out)
        (let ((l (uint-length-in-u8 n)))
          (if (<= l 63)
            (begin
              (write-u8 (+ l 63) out)
              (write-uint-u8vector n l out))
            (begin
              (write-u8 127 out)
              (write-varint l out)
              (write-uint-u8vector n l out)))))))

;; Encoding and decoding natural integers into self-delimited byte streams, preserving lexicographic order.
;; : UInt <- In
(def (read-varuint in)
  (let ((x (read-u8 in)))
    (cond
     ((< x 128) x)
     ((< x 255) (let* ((l (- x 127))
                       (n (read-uint-u8vector l in)))
                    (assert! (> n 127))
                    (assert! (= (uint-length-in-u8 n) l))
                    n))
     (else ; (= x 255)
      (let* ((l (read-varuint in))
             (n (read-uint-u8vector l in)))
          (assert! (> l 127))
          (assert! (= (uint-length-in-u8 n) l))
          n)))))

;; : <- UInt Out
(def (write-varuint n out)
  (if (<= n 127) (write-u8 n out)
      (let ((l (uint-length-in-u8 n)))
        (if (<= l 127)
          (begin
            (write-u8 (+ l 127) out)
            (write-uint-u8vector n l out))
            (begin
              (write-u8 255 out)
              (write-varuint l out)
              (write-uint-u8vector n l out))))))
