;; -*- Gerbil -*-
;;;; Basic LL(1) parsers

;; TODO: parsing combinators that produce generating functions for all the values of a parse
;; from a generator (or stream?) of values?
;; OR, combinators that use interface-passing to handle the specific

;; TODO: parse from buffers with indefinite lookahead?
;; Just use vyzo's grammars?

(export #t)

(import
  (for-syntax :std/stxutil)
  :gerbil/gambit/bytes
  :scheme/base-ports :scheme/char
  :std/error :std/iter :std/misc/list-builder :std/srfi/13 :std/sugar
  ./base)

;; NB: This assumes Latin / English alphabet

(defsyntax (def-ascii stx)
  (syntax-case stx ()
    ((d (name x y ...) body ...)
     (with-syntax ((char-fun (format-id #'name "char-ascii-~a" #'name))
                   (byte-fun (format-id #'name "byte-ascii-~a" #'name)))
       #'(begin
           (def (byte-fun x y ...) (declare (fixnum)) body ...)
           (def (char-fun x y ...) (and (char? x) (byte-fun (char->integer x) y ...))))))))

(def-ascii (alphabetic? b) (or (<= 65 b 90) #|A-Z|# (<= 97 b 122) #|a-z|#))
(def-ascii (numeric? b) (<= 48 b 57)) #|0-9|#
(def-ascii (alphanumeric? b) (or (byte-ascii-alphabetic? b) (byte-ascii-numeric? b)))
(def-ascii (alphanumeric-or-underscore? b) (or (byte-ascii-alphanumeric? b) (= b 95)))
(def-ascii (graphic? b) (<= 32 b 127))
(def-ascii (whitespace? b)
  (or (= b #x20) ;; #\space
      (= b #x09) ;; #\tab
      (= b #x0A) ;; #\newline
      (= b #x0C) ;; #\page
      (= b #x0D))) ;; #\return
(def-ascii (printable? b) (or (byte-ascii-graphic? b) (byte-ascii-whitespace? b)))
;; Assume ASCII, base 2 to 36
(def (byte-ascii-digit b (base 10))
  (let (found (lambda (d) (and (< d base) d)))
    (cond
     ((<= 48 b 57) (found (- b 48))) ;; ASCII 0-9
     ((<= 65 b 90) (found (- b 55))) ;; ASCII A-Z
     ((<= 97 b 122) (found (- b 87))) ;; ASCII a-z
     (else #f))))
(def (char-ascii-digit c (base 10))
  (and (char? c) (byte-ascii-digit (char->integer c) base)))

(def (char-port-eof? port) (eof-object? (peek-char port)))
(def (byte-port-eof? port) (eof-object? (peek-u8 port)))

(def (bytes-every pred b)
  (let/cc return
    (for (i (in-range (bytes-length b)))
      (unless (pred (bytes-ref b i)) (return #f)))
    #t))
(def (bytes-ascii-printable? b)
  (and (bytes? b) (bytes-every byte-ascii-printable? b)))

;;; Parse error
(defstruct (parse-error Exception) (message args where) transparent: #t)
(def (parse-error! where message . args) (raise (make-parse-error message args where)))

;;; Expect a natural number in decimal on the current port, return it.
(def (expect-natural port (base 10))
  (if-let (digit (char-ascii-digit (peek-char port) base))
     (let loop ((n digit))
       (read-char port)
       (if-let (next-digit (char-ascii-digit (peek-char port) base))
         (loop (+ next-digit (* base n)))
         n))
    (parse-error! 'expect-natural "Not a digit in requested base" (peek-char port) base port)))

(def (expect-signed-integer port (base 10))
  (let ((char (peek-char port)))
    (cond
     ((eqv? char #\+)
      (read-char port)
      (expect-natural port base))
     ((eqv? char #\-)
      (read-char port)
      (- (expect-natural port base)))
     ((char-ascii-digit char)
      (expect-natural port base))
     (else
      (parse-error! 'expect-signed-integer "Neither a sign nor a digit in requested base"
                    char base port)))))

(def (expect-maybe-one-of char-pred?)
  (λ (port)
    (and (char-pred? (peek-char port))
         (read-char port))))

(def (expect-one-of char-pred?)
  (λ (port)
    (if (char-pred? (peek-char port))
      (read-char port)
      (parse-error! 'expect-one-of "Unexpected character" (peek-char port) char-pred? port))))

(def (expect-any-number-of char-pred?)
  (λ (in)
    (and (char-pred? (peek-char in))
         (call-with-output-string
          (lambda (out) (while (begin (write-char (read-char in) out) (char-pred? (peek-char in)))))))))

(def (expect-one-or-more-of char-pred?)
  (λ (in)
    (or ((expect-any-number-of char-pred?) in)
        (parse-error! 'expect-one-or-more-of "Unexpected character" (peek-char in) in))))

(def (expect-maybe-char char)
  (expect-maybe-one-of (cut eqv? char <>)))

(def (expect-char char)
  (expect-one-of (cut eqv? char <>)))

(def (expect-and-skip-any-whitespace port)
  (while (char-ascii-whitespace? (peek-char port))
    (read-char port)))

(def expect-eof (expect-one-of eof-object?))

(def (eol-char? x)
  (or (eqv? x #\newline) (eqv? x #\return) (eof-object? x)))

(def (expect-eol port)
  (def char ((expect-one-of eol-char?) port))
  (when (eqv? char #\return)
    ((expect-maybe-char #\newline) port)))

(def (expect-literal-string string)
  (λ (port)
    (string-for-each (λ (c) ((expect-char c) port)) string)))

(def (expect-n-digits n (base 10))
  (λ (port)
    (let loop ((n n) (r 0))
      (if (zero? n) r
          (let* ((char (peek-char port))
                 (digit (char-ascii-digit char base)))
            (if digit
              (begin (read-char port) (loop (- n 1) (+ digit (* base r))))
              (parse-error! 'expect-n-digits "not a digit" char base port)))))))

;; Like read-line, but handles any of the CRLF, CR and LF line endings
(def (expect-line port)
  (call-with-output-string
    [] (λ (out)
         (let loop ()
           (let ((char (peek-char port)))
             (cond
              ((eol-char? char) (expect-eol port))
              ((eof-object? char) (void))
              (else (display char out) (read-char port) (loop))))))))

(def (expect-lines parse-line port)
  (nest
   (with-list-builder (c))
   (until (char-port-eof? port))
   (begin (c (parse-line port)))
   (expect-eol port)))

(def (expect-then-eof expecter)
  (lambda (in) (begin0 (expecter in) (expect-eof in))))

(def (parse-port port parser (description port) (where 'parse-port))
  (with-catch (lambda (e) (parse-error! where "failure parsing" description (error-message e)))
              (lambda () ((expect-then-eof parser) port))))

(def (parse-file file parser (description file) (where 'parse-file))
  (call-with-input-file file (lambda (port) (parse-port port parser description where))))

(def (parse-string string parser (description string) (where 'parse-string))
  (call-with-input-string string (lambda (port) (parse-port port parser description where))))

(def (parse-file-lines parse-line file (description file) (where 'parse-file-lines))
  (parse-file file (cut expect-lines parse-line <>) description where))

(def ((expect-alternatives alternatives (where 'expect-alternatives)) port)
  (let loop ((as alternatives))
    (if (null? as)
      (parse-error! where "none applied" alternatives port)
      (with-catch (λ (e) (if (parse-error? e) (loop (cdr as)) (raise e)))
                  (lambda () ((car as) port))))))

;; Monadic parsing combinators
(defrule (expect-result result) (lambda (_port) result))
(def (expect-pure value) (expect-result value))
(def (expect-bind processed processor)
  (lambda (port) ((processor (processed port)) port)))
(def (expect-or . alternatives)
  (expect-alternatives alternatives 'expect-or))
(defrule (expect-begin expect-ignored ... expect-value)
  (λ (port) (expect-ignored port) ... (expect-value port)))
(defrule (expect-begin0 expect-value expect-ignored ...)
  (λ (port) (begin0 (expect-value port) (expect-ignored port) ...)))
(def (expect-repeated expect-element expect-terminator (rhead '()))
  (let loop ((r rhead))
    (expect-or
     (expect-begin expect-terminator (expect-result (reverse r)))
     (expect-bind expect-element (lambda (e) (loop [e . r]))))))
(def (expect-separated expect-element expect-separator expect-terminator)
  (expect-or
   (expect-begin expect-terminator (expect-pure '()))
   (expect-bind expect-element
                (lambda (e) (expect-repeated (expect-begin expect-separator expect-element)
                                        expect-terminator [e])))))
(def ((expect-n-repeats n expect-element) port)
  (for/collect ((_ (in-range n))) (expect-element port)))
