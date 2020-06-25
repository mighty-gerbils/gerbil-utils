(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/iter :std/sugar
  ../utils/base ../utils/io
  ./poo ./mop ./brace)

;; Byte <- In
(.defgeneric (poo-read-byte in)
  default:
  (λ (in)
    (if (input-port? in) (read-byte in)
        (error "Trying to read-byte from unsupported object" in))))

;; Unit <- Byte Out
(.defgeneric (poo-write-byte byte out))

;; Unit <- In Bytes ?offset: Nat ?length: Nat
(.defgeneric (read-bytes-into in bs offset: (offset 0) length: (length (- (bytes-length bs) offset)))
   default:
   (λ (in l)
    (for (i (in-range length))
      (let ((b (read-byte in))) ;; TODO: handle EOF, return number of bytes read???
        (bytes-set! bs (+ i offset) b)))))

;; Bytes <- In Nat
(.defgeneric (poo-read-bytes in length)
  default:
  (λ (in length)
    (def bs (make-bytes length))
    (if (input-port? in)
      (let ((n (read-bytes bs in))) (assert! (= n length)))
      (read-bytes-into in bs length: length))
    bs))

;; Unit <- Bytes Out ?offset: Nat ?length: Nat
(.defgeneric (poo-write-bytes bs out offset: (offset 0) length: (length (- (bytes-length bs) offset)))
   default:
   (λ (out bs offset: (offset 0) length: (length (- (bytes-length bs) offset)))
     (for (i (in-range length))
       (write-byte (bytes-ref bs (+ i offset)) out))))

(.defgeneric (marshal type x port)
   slot: .marshal from: methods)

(.defgeneric (unmarshal type port)
   slot: .unmarshal from: methods)

;; : Bytes <- 'a:Type 'a
(.defgeneric (bytes<- type x)
   slot: .bytes<- from: methods)

;; : 'a <- 'a:Type Bytes
(.defgeneric (<-bytes type b)
   slot: .<-bytes from: methods)

(.def (bytes<-un/marshal @ [] .marshal .unmarshal)
   .bytes<-: (bytes<-<-marshal .marshal)
   .<-bytes: (<-bytes<-unmarshal .unmarshal))

(.def (un/marshal<-bytes @ [] .<-bytes .bytes<- length-in-bytes)
   .marshal: (marshal<-bytes<- .bytes<-)
   .unmarshal: (unmarshal<-<-bytes .<-bytes length-in-bytes))
