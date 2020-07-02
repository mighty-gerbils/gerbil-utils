(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/hash :gerbil/gambit/ports
  :std/generic :std/iter :std/misc/repr :std/sugar :std/text/json
  ../utils/base ../utils/io ../utils/json
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

(defmethod (@@method :pr poo)
  (λ (self (port (current-output-port)) (options (current-representation-options)))
    (cond
     ((.has? self .type print-object) => (lambda (m) (m self port options)))
     ((.has? self .type methods .sexp<-) => (lambda (m) (write (m self) port)))
     ((.has? self .type) (print-class-object self port options))
     ((.has? self :pr) => (lambda (m) (m port options)))
     ((.has? self sexp) => (lambda (s) (write s port)))
     (else (print-unrepresentable-object self port options)))))

(.defgeneric (sexp<- type x) from: methods slot: .sexp<-)
(.defgeneric (<-json type j) from: methods slot: .<-json)
(.defgeneric (json<- type x) from: methods slot: .json<-)

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

(def (print-class-object
      x (port (current-output-port)) (options (current-representation-options)))
  (def (d x) (display x port))
  (def (w x) (write x port))
  (d "(begin0 #") (d (object->serial-number x)) (d " {")
  (try
   (for-each (λ-match ([k . v] (d " (") (w k) (d " ") (prn v) (d ")"))) (.alist x))
   (catch (e) (void)))
  (d "})"))

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

(.defgeneric (printable-slots x)
   from: type
   default: .all-slots
   slot: .printable-slots)

(def (@@method :json poo) (json<- (.@ poo .type) poo))

(def (json-string<- type x)
  (string<-json (json<- type x)))
