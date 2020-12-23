(export #t)
(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/misc/bytes :std/sugar
  ./base ./number)

;;(def (write-u8vector v p) (write-subu8vector v 0 (u8vector-length v) p))
;;(def (read-u8vector v p) (def l (u8vector-length v)) (read-subu8vector v 0 l p l))

;; : UInt16 <- In
(def (read-uint16 port) ;; big endian
  (def hi (read-u8 port))
  (def lo (read-u8 port))
  (fx+ lo (fxarithmetic-shift hi 8)))

;; : Bytes <- In
(def (read-sized16-bytes port)
  (def size (read-uint16 port))
  (read-bytes* size port))

;; Read a given number of bytes, even if the number is zero
;; : Bytes <- Nat In
(def (read-bytes* size port)
  (if (zero? size)
    #u8()
    (read-bytes size port)))

;; : <- UInt16 Out
(def (write-uint16 n port)
  (assert! (<= 0 n 65535))
  (def bytes (make-bytes 2))
  (bytevector-u16-set! bytes 0 n big)
  (write-bytes bytes port))

;; : <- Bytes Out
(def (write-sized16-bytes bytes port)
  (write-uint16 (bytes-length bytes) port)
  (write-bytes bytes port))

;; : (Bytes <- 'a) <- (<- 'a Out)
(def (bytes<-<-marshal marshal)
  (lambda (x) (call-with-output-u8vector (lambda (port) (marshal x port)))))

;; : ('a <- Bytes) <- ((OrEof 'a) <- In)
(def (<-bytes<-unmarshal unmarshal)
  (nest (lambda (bytes)) (call-with-input-u8vector bytes) (lambda (port))
        (let ((v (unmarshal port)))
          (assert! (not (eof-object? v)))
          (assert! (eof-object? (read-u8 port)))
          v)))

;; : (<- 'a Out) <- (Bytes <- 'a)
(def (marshal<-bytes<- bytes<-)
  (lambda (x port) (write-u8vector (bytes<- x) port)))

;; : ('a <- In) <- ('a <- Bytes) Nat
(def (unmarshal<-<-bytes <-bytes n)
  (lambda (port) (eofmap <-bytes (read-bytes* n port))))

;; eofmap : [A -> B] [OrEof A] -> [OrEof B]
(def (eofmap f m) (if (eof-object? m) m (f m)))

;; : Nat <- In Nat+
(def (read-integer-bytes length-in-bytes in)
  (nat<-bytes (read-bytes* length-in-bytes in)))

;; : <- Int Nat+ Out
(def (write-integer-bytes x length-in-bytes out)
  (write-bytes (bytes<-nat x length-in-bytes) out))

;; Encoding and decoding integers into self-delimited byte streams, preserving lexicographic order
;; supposing the first byte is compared signed and the rest unsigned.
;; : Int <- In
(def (read-varint in)
  (let ((x (read-byte in)))
    (if (< x 128)
      (cond
       ((< x 64) x)
       ((< x 127) (let* ((l (- x 63))
                         (n (read-integer-bytes l in)))
                    (assert! (>= n 64))
                    (assert! (= (integer-length-in-bytes n) l))
                    n))
       (else ; (= x 127)
        (let* ((l (read-varint in))
               (n (read-integer-bytes l in)))
          (assert! (>= l 64))
          (assert! (= (integer-length-in-bytes n) l))
          n)))
      (cond
       ((>= x 192) (- x 256))
       ((> x 128) (let* ((l (- 192 x))
                         (n (bitwise-ior (arithmetic-shift -1 l) (read-integer-bytes l in))))
                    (assert! (< n -64))
                    (assert! (= l (integer-length-in-bytes n)))
                    n))
       (else ; (= x 128)
        (let* ((l (- (read-varint in)))
               (n (bitwise-ior (arithmetic-shift -1 l) (read-integer-bytes l in))))
          (assert! (> l 63))
          (assert! (= l (integer-length-in-bytes n)))
          n))))))

;; : <- Int Out
(def (write-varint n out)
  (if (negative? n)
    (if (>= n -64) (write-byte (bitwise-and 255) out)
        (let ((l (integer-length-in-bytes n)))
          (if (<= 63)
            (begin
              (write-byte (- 192 l) out)
              (write-integer-bytes n l out))
            (begin
              (write-byte 128 out)
              (write-varint (- l) out)
              (write-integer-bytes n l out)))))
    (if (<= n 63) (write-byte n out)
        (let ((l (integer-length-in-bytes n)))
          (if (<= l 63)
            (begin
              (write-byte (+ l 63) out)
              (write-integer-bytes n l out))
            (begin
              (write-byte 127 out)
              (write-varint l out)
              (write-integer-bytes n l out)))))))

;; Encoding and decoding natural integers into self-delimited byte streams, preserving lexicographic order.
;; : Nat <- In
(def (read-varnat in)
  (let ((x (read-byte in)))
    (cond
     ((< x 128) x)
     ((< x 255) (let* ((l (- x 127))
                       (n (read-integer-bytes l in)))
                    (assert! (> n 127))
                    (assert! (= (integer-length-in-bytes n) l))
                    n))
     (else ; (= x 255)
      (let* ((l (read-varnat in))
             (n (read-integer-bytes l in)))
          (assert! (> l 127))
          (assert! (= (integer-length-in-bytes n) l))
          n)))))

;; : <- Nat Out
(def (write-varnat n out)
  (if (<= n 127) (write-byte n out)
      (let ((l (integer-length-in-bytes n)))
        (if (<= l 127)
          (begin
            (write-byte (+ l 127) out)
            (write-integer-bytes n l out))
            (begin
              (write-byte 255 out)
              (write-varnat l out)
              (write-integer-bytes n l out))))))
