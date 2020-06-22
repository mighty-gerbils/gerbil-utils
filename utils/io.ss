(export #t)
(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/misc/bytes :std/sugar
  :clan/utils/base :clan/utils/number)

;;(def (write-u8vector v p) (write-subu8vector v 0 (u8vector-length v) p))
;;(def (read-u8vector v p) (def l (u8vector-length v)) (read-subu8vector v 0 l p l))

(def (read-uint16 port) ;; big endian
  (def hi (read-u8 port))
  (def lo (read-u8 port))
  (fx+ lo (fxarithmetic-shift hi 8)))

(def (read-sized16-bytes port)
  (def size (read-uint16 port))
  (read-bytes size port))

(def (write-uint16 n port)
  (assert! (<= 0 n 65535))
  (def bytes (make-bytes 2))
  (bytevector-u16-set! bytes 0 n big)
  (write-bytes bytes port))

(def (write-sized16-bytes bytes port)
  (write-uint16 (bytes-length bytes) port)
  (write-bytes bytes port))

(def (bytes<-<-marshal marshal)
  (nest (lambda (bytes)) (call-with-output-u8vector) (lambda (port))
        (marshal bytes port)))

(def (<-bytes<-unmarshal unmarshal)
  (nest (lambda (bytes)) (call-with-input-u8vector bytes) (lambda (port))
        (begin0 (unmarshal port))
        (assert! (eq? #!eof (read-u8 port)))))

(def (marshal<-bytes<- bytes<-)
  (lambda (x port) (write-u8vector (bytes<- x) port)))

(def (unmarshal<-<-bytes <-bytes n)
  (lambda (port) (<-bytes (read-bytes n port))))

;; Nat <- In Nat
(def (read-integer-bytes in l)
  (nat<-bytes (read-bytes in l)))

;; Unit <- Out Int Nat
(def (write-integer-bytes out n l)
  (write-bytes (bytes<-nat n l) out))

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
