(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/hash :gerbil/gambit/ports
  :std/generic :std/iter :std/misc/repr :std/sugar :std/text/json
  ../utils/base ../utils/hash ../utils/io ../utils/json
  ./poo ./mop ./brace)

;;; Byte I/O

;; gf to effectfully read a byte from a stream or stream-like object.
;; Byte <- In
(.defgeneric (poo-read-byte in)
  default:
  (λ (in)
    (if (input-port? in) (read-byte in)
        (error "Trying to read-byte from unsupported object" in))))

;; gf to effectfully write a byte to a stream or stream-like object.
;; Unit <- Byte Out
(.defgeneric (poo-write-byte byte out))

;; gf to effectfully read bytes from a stream or stream-like object into a u8vector
;; Unit <- In Bytes ?offset: Nat ?length: Nat
(.defgeneric (read-bytes-into in bs offset: (offset 0) length: (length (- (bytes-length bs) offset)))
   default:
   (λ (in l)
    (for (i (in-range length))
      (let ((b (read-byte in))) ;; TODO: handle EOF, return number of bytes read???
        (bytes-set! bs (+ i offset) b)))))

;; gf to effectfully read bytes from a stream or stream-like object into a new u8vector
;; Bytes <- In Nat
(.defgeneric (poo-read-bytes in length)
  default:
  (λ (in length)
    (def bs (make-bytes length))
    (if (input-port? in)
      (let ((n (read-bytes bs in))) (assert! (= n length)))
      (read-bytes-into in bs length: length))
    bs))

;; gf to effectfully write bytes to a stream or stream-like object from a u8vector
;; Unit <- Bytes Out ?offset: Nat ?length: Nat
(.defgeneric (poo-write-bytes bs out offset: (offset 0) length: (length (- (bytes-length bs) offset)))
   default:
   (λ (out bs offset: (offset 0) length: (length (- (bytes-length bs) offset)))
     (for (i (in-range length))
       (write-byte (bytes-ref bs (+ i offset)) out))))


;;; SEXP from values

;; gf to extract a source sexp from a value of given type
(.defgeneric (sexp<- type x) from: methods slot: .sexp<-)

(defgeneric :sexp
  (lambda (x)
    (cond
     ((or (number? x) (boolean? x) (string? x) (char? x) (void? x) (keyword? x) (eof-object? x))
      x)
     (else `',x)))) ;; TODO: do better than that.

(defmethod (@@method :sexp poo)
  (λ (self)
    (cond
     ((.has? self .type methods .sexp<-) (.call (.@ self .type methods) .sexp<- self))
     ((.has? self sexp) (object->string (.@ self sexp))))))


;;; Printing objects as per std/misc/repr

(defmethod (@@method :pr poo)
  (λ (self (port (current-output-port)) (options (current-representation-options)))
    (cond
     ((.has? self .type print-object) => (lambda (m) (m self port options)))
     ((.has? self .type methods .sexp<-) => (lambda (m) (write (m self) port)))
     ((.has? self .type) (print-class-object self port options))
     ((.has? self :pr) => (lambda (m) (m port options)))
     ((.has? self sexp) => (lambda (s) (write s port)))
     (else (print-unrepresentable-object self port options)))))

(def (print-class-object
      x (port (current-output-port)) (options (current-representation-options)))
  (def (d x) (display x port))
  (def (w x) (write x port))
  (d "(begin0 #") (d (object->serial-number x)) (d " {")
  (try
   (for-each (λ-match ([k . v] (d " (") (w k) (d " ") (prn v) (d ")"))) (.alist x))
   (catch (e) (void)))
  (d "})"))

(.defgeneric (printable-slots x) from: type default: .all-slots slot: .printable-slots)


;;; JSON I/O

;; gf to extract a value of given type from some json
(.defgeneric (<-json type j) from: methods slot: .<-json)

;; gf to extract some json from a value of given type
(.defgeneric (json<- type x) from: methods slot: .json<-)

(def (@@method :json poo) (json<- (.@ poo .type) poo))

(def (json-string<- type x)
  (string<-json (json<- type x)))
(def (<-json-string type x)
  (<-json type (json<-string x)))

(.def (methods.string<-json @ [] .json<- .<-json)
  .string<-: (compose string<-json .json<-)
  .<-string: (compose .<-json json<-string))


;;; (Un)Marshaling data to/from bytes
;; The output has to be self-delimited, so marshaling output can be concatenated given the types.
;; A trivial way to satisfy this constraint is for the output to have fixed length,
;; or for the length to be prepended. Another way is to have some terminator value (e.g. 0 in C strings).

(.defgeneric (marshal type x port)
   slot: .marshal from: methods)

(.defgeneric (unmarshal type port)
   slot: .unmarshal from: methods)


;;; Converting to/from bytes
;; Unlike with marshaling, these bytes do NOT have to be self-delimited in case length is variable.

;; : Bytes <- 'a:Type 'a
(.defgeneric (bytes<- type x)
   slot: .bytes<- from: methods)

;; : 'a <- 'a:Type Bytes
(.defgeneric (<-bytes type b)
   slot: .<-bytes from: methods)

(.def (methods.bytes<-marshal @ [] .marshal .unmarshal)
   .bytes<-: (bytes<-<-marshal .marshal)
   .<-bytes: (<-bytes<-unmarshal .unmarshal))

(.def (methods.marshal<-bytes @ [] .<-bytes .bytes<- .Bytes)
   .marshal: (lambda (x port) (marshal .Bytes (.bytes<- x) port))
   .unmarshal: (lambda (port) (.<-bytes (unmarshal .Bytes port))))

(.def (methods.marshal<-fixed-length-bytes @ [] .<-bytes .bytes<- length-in-bytes)
   .marshal: (lambda (x port) (write-bytes (.bytes<- x) port))
   .unmarshal: (lambda (port) (.<-bytes (read-bytes length-in-bytes port))))

;;; Converting to/from string

;; : String <- 'a:Type 'a
(.defgeneric (string<- type x)
   slot: .string<- from: methods)

;; : 'a <- 'a:Type String
(.defgeneric (<-string type b)
   slot: .<-string from: methods)

