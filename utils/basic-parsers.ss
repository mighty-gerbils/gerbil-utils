;; -*- Gerbil -*-
;;;; Basic parsers

;; TODO: parsing combinators that produce generating functions for all the values of a parse
;; from a generator (or stream?) of values?
;; OR, combinators that use interface-passing to handle the specific

;; TODO: parse from buffers with indefinite lookahead?
;; Just use vyzo's grammars?

(export #t)

(import
  :scheme/char
  :std/error :std/srfi/13 :std/sugar
  :utils/base)

(def (numeric-char? x)
  (and (char? x) (char-numeric? x)))

(def (whitespace-char? x)
  (and (char? x) (char-whitespace? x)))

;; NB: This assumes Latin / English alphabet
(def (alphabetic-char? x)
  (and (char? x) (char-alphabetic? x)))

(def (alphanumeric-char? x)
  (and (char? x) (or (char-alphabetic? x) (char-numeric? x))))

(def (ascii-graphic-char? x)
  (and (char? x) (<= 32 (char->integer x) 127)))

;; Assume ASCII, base 2 to 36
;; : (Or Integer '#f) <- Char Integer
(def (char-digit x (base 10))
  ;; (assert! (or (char? x) (eof-object? x)))
  ;; (assert! (and (exact-integer? base) (<= 2 base 36)))
  (and (char? x)
       (let ((n (char->integer x))
             (found (λ (d) (and (< d base) d))))
         (cond
          ((<= 48 n 57) (found (- n 48))) ;; ASCII 0-9
          ((<= 65 n 90) (found (- n 55))) ;; ASCII A-Z
          ((<= 97 n 122) (found (- n 87))) ;; ASCII a-z
          (else #f)))))

(def (port-eof? port) (eof-object? (peek-char port)))


;;; Parse error
(defstruct (parse-error <error>) ())
(def (parse-error! where message . args) (raise (make-parse-error message args where)))

;;; Expect a natural number in decimal on the current port, return it.
(def (expect-natural port (base 10))
  (if-let (digit (char-digit (peek-char port) base))
     (let loop ((n digit))
       (read-char port)
       (if-let (next-digit (char-digit (peek-char port) base))
         (loop (+ next-digit (* base n)))
         n))
    (parse-error! 'expect-natural "Not a digit in requested base" (peek-char port) base port)))

(def (expect-maybe-one-of char-pred?)
  (λ (port)
    (and (char-pred? (peek-char port))
         (read-char port))))

(def (expect-one-of char-pred?)
  (λ (port)
    (if (char-pred? (peek-char port))
      (read-char port)
      (parse-error! 'expect-one-of "Unexpected character" (peek-char port) port))))

(def (expect-maybe-char char)
  (expect-maybe-one-of (cut eqv? char <>)))

(def (expect-char char)
  (expect-one-of (cut eqv? char <>)))

(def (expect-and-skip-any-whitespace port)
  (while (whitespace-char? (peek-char port))
    (read-char port)))

(def expect-eof (expect-one-of eof-object?))

(def (eol-char? x)
  (or (eqv? x #\newline) (eqv? x #\return)))

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
                 (digit (char-digit char base)))
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
