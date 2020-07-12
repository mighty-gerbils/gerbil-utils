(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/exact
  :scheme/base-impl
  :std/error :std/iter :std/sugar
  :clan/utils/basic-parsers :clan/utils/basic-printers :clan/utils/number :clan/utils/string)

;; : (Parameter Char)
(def decimal-mark (make-parameter #\.))

;; Precision loss mode. Must be 'error, 'round, or 'truncate
;; : (Parameter Symbol)
(def precision-loss-behavior (make-parameter 'error))

;; : Bool <- Any
(def (decimal? x)
  (and (rational? x)
       (power-of-5? (factor-out-powers-of-2 (denominator x)))))

;; : Integer <- Integer
(def (factor-out-powers-of-2 n)
  (arithmetic-shift n (- (first-bit-set n))))

;; : Bool <- Integer
(def (power-of-5? n)
  (def l (integer-part (round (log n 5))))
  (= n (expt 5 l)))

(defrule (repeat n body ...)
  (let loop ((i n)) (when (<= 1 i) body ... (loop (1- i)))))
(def (write-n-chars n char port)
  (repeat n (write-char char port)))

;; `expect-decimal` expects and parses a decimal number on the port.
;; The character parameters `decimal-mark` and `group-separator` provide
;; support for different (typically cultural) numerical conventions.
;; For convenience, a `group-separator` of #t will be treated as the comma character.
;; The boolean `sign-allowed` controls signage.
;; `exponent-allowed` is a boolean or a sequence controlling exponent notation.
;; Exponent notation follows the syntax for Scheme floats,
;; with exception that the exponent marker must be 'e' or 'E' when `exponent-allowed` is #t,
;; or the exponent marker must be `char=` to some element of `exponent-allowed`
;; when `exponent-allowed` is a string.
;; Side-effects the port, and returns the decimal number, or raises an exception.
;; It is up to the caller to ignore and leading or trailing whitespace and check for eof
;; before and/or after calling `expect-decimal`.
;; : Decimal <- Port sign-allowed?:Bool decimal-mark:Char group-separator:(Or Char Bool) exponent-allowed:(or Bool String)
(def (expect-decimal
      port
      sign-allowed?: (sign-allowed? #t)
      decimal-mark: (decimal-mark #\.)
      group-separator: (group-separator_ #f)
      exponent-allowed: (exponent-allowed_ #f))
  (assert! (input-port? port))
  (assert! (boolean? sign-allowed?))
  (assert! (char? decimal-mark))
  (assert! (or (boolean? group-separator_) (char? group-separator_)))
  (assert! (or (boolean? exponent-allowed_) (string? exponent-allowed_)))
  (def group-separator (if (eq? group-separator_ #t) #\, group-separator_))
  (def exponent-allowed (if (eq? exponent-allowed_ #t) "eE" exponent-allowed_))
  (assert! (not (and decimal-mark group-separator (char=? decimal-mark group-separator))))
  (assert! (not (and exponent-allowed (string-index exponent-allowed decimal-mark))))
  (assert! (not (and group-separator exponent-allowed (string-index exponent-allowed group-separator))))
  (def numerator 0)
  (def denominator 1)
  (def sign 1)
  (def exponent 0)
  (def exponent-sign 1)
  (def valid? #f) ;; have we seen at least one digit
  (def c (peek-char port))
  (def (bad) (raise (parse-error 'expect-decimal "Unexpected character" port)))
  (def (next) (read-char port) (set! c (peek-char port)))
  (def (expect-sign) (case c ((#\+) (next) 1) ((#\-) (next) -1) (else 1)))
  (let/cc ret
    (when (eof-object? c) (parse-error! 'expect-decimal "Unexpected EOF" port))
    (when sign-allowed? (set! sign (expect-sign)))
    (def (done) (ret (* sign (/ numerator denominator) (expt 10 (* exponent-sign exponent)))))
    (def (expect-left-digit-or-group-separator)
      (cond
       ((char-ascii-digit c) =>
        (lambda (d)
          (set! numerator (+ (* numerator 10) d))
          (set! valid? #t)
          (next)
          (expect-left-digit-or-group-separator)))
       ((and group-separator (eqv? group-separator c))
        (next)
        (expect-left-digit-or-group-separator))
       (else (maybe-decimal-mark))))
    (def (maybe-decimal-mark)
      (cond
       ((eqv? c decimal-mark)
        (next)
        (if valid? ;; if we have seen a left digit
          (maybe-right-digit)
          (expect-right-digit)))
       (valid?
        (maybe-exponent-marker))
       (else
        (bad))))
    (def (expect-right-digit)
      (cond
       ((char-ascii-digit c) =>
        (lambda (d)
          (set! numerator (+ (* numerator 10) d))
          (set! denominator (* denominator 10))
          (set! valid? #t)
          (next)
          (maybe-right-digit)))
       (else (bad))))
    (def (maybe-right-digit)
      (cond
       ((char-ascii-digit c) =>
        (lambda (d)
          (set! numerator (+ (* numerator 10) d))
          (set! denominator (* denominator 10))
          (set! valid? #t)
          (next)
          (maybe-right-digit)))
       (else (maybe-exponent-marker))))
    (def (maybe-exponent-marker)
      (cond
       ((and exponent-allowed (string-index exponent-allowed c))
        (set! valid? #f) ;; invalid until we finish parsing the exponent
        (next)
        (maybe-exponent-sign))
       (else (done))))
    (def (maybe-exponent-sign)
      (set! exponent-sign (expect-sign))
      (expect-exponent-digit))
    (def (expect-exponent-digit)
      (cond
       ((char-ascii-digit c) =>
        (lambda (d)
          (set! exponent (+ (* exponent 10) d))
          (set! valid? #t)
          (next)
          (maybe-exponent-digit)))
        (else (bad))))
    (def (maybe-exponent-digit)
      (cond
       ((char-ascii-digit c) =>
        (lambda (d)
          (set! exponent (+ (* exponent 10) d))
          (set! valid? #t)
          (next)
          (maybe-exponent-digit)))
       (else
        (done))))
    (expect-left-digit-or-group-separator)))

;; Decimal <- String sign-allowed?:Bool decimal-mark:Char group-separator:(Or Char Bool) exponent-allowed:(or Bool String) allow-leading-whitespace?:Bool allow-trailing-whitespace?:Bool start:Nat end:(OrFalse Nat)
(def (decimal<-string s
      sign-allowed?: (sign-allowed? #t)
      decimal-mark: (decimal-mark #\.)
      group-separator: (group-separator #f)
      exponent-allowed: (exponent-allowed #f)
      allow-leading-whitespace?: (allow-leading-whitespace? #f)
      allow-trailing-whitespace?: (allow-trailing-whitespace? #f)
      start: (start 0)
      end: (end_ #f))
  (def l (string-length s))
  (def end (or end_ l))
  (call-with-input-string
   (if (and (zero? start) (= end l)) s (substring s start end))
   (lambda (port)
     (when allow-leading-whitespace?
       (expect-and-skip-any-whitespace port))
     (begin0
         (expect-decimal port
                         sign-allowed?: sign-allowed?
                         decimal-mark: decimal-mark
                         group-separator: group-separator
                         exponent-allowed: exponent-allowed)
       (when allow-trailing-whitespace?
         (expect-and-skip-any-whitespace port))
       (expect-eof port)))))

;; Given an integer d of the form 2^m*5^n (reduced denominator of a decimal number),
;; compute c such that c*d = c*(2^m*5^n) = 10^max(m,n).
;; Returns c and max(m,n).
;; : Nat Nat <- Nat
(def (find-multiplier d)
  (def m (first-bit-set d))
  (def 5^n (arithmetic-shift d (- m)))
  (def n (integer-part (round (log 5^n 5))))
  ;; Log and integer-length seem to work perfectly as used above even for
  ;; extremely large numbers. However, in case of any incorrectness
  ;; due to precision, portability issues, etc., we check that the answer is
  ;; fully correct before returning it to the caller.
  (assert! (= d (* (expt 2 m) (expt 5 n))))
  (if (> m n)
    (values (expt 5 (- m n)) m)
    (values (expt 2 (- n m)) n)))

;; : Nat+ <- Integer
(def (count-digits n)
  (assert! (exact-integer? n))
  (if (zero? n)
      1
      ;; Could use log instead but it might not be safe in every case
      (let loop ((l 0) (a (abs n)))
        (if (< a 1) l (loop (1+ l) (quotient a 10))))))

;; Note that the exponent is a natural.
;; Maybe we should find the valuation of the decimal in base 10,
;; and return the least, negative, number when appropriate?
(def (digits-exponent<-decimal decimal)
  (defvalues (c m) (find-multiplier (denominator decimal)))
  (values (* (numerator decimal) c) m))

;; From an integer number for the digits and an exponent for the negative powers of 10,
;; return a decimal number.
(def (decimal<-digits-exponent digits exponent)
  (/ digits (expt 10 exponent)))

;; Converts an integer into a base 10 string. The sign is ignored.
;; : String <- Integer
(def (digits<-integer n)
  (assert! (exact-integer? n))
  (if (zero? n)
    "0"
    (let* ((digit-count (count-digits n))
           (str (make-string digit-count))
           (remainder 0))
      (let loop ((a (abs n))
                 (i (1- digit-count)))
        (if (minus? i) str
            (let-values (((q r) (truncate/ a 10)))
              (string-set! str i (digit-char r 10))
              (loop q (1- i))))))))

;; Attempted print operation would lose precision. See precision-loss-behavior.
(defstruct (disallowed-loss-of-precision exception) ())

;; Given a decimal number, a position (integer or false), and a boolean,
;; Returns a string of digits, and the position of the decimal mark relative to the start of that string,
;; so that 0 means "just before the string digits", -4 means "four positions before the string digits",
;; 3 means "3 positions after the first digit".
;; The input position, if not false, indicates how many fractional digits are wanted.
;; The boolean true means the digits are relative to the most significant digit of the number,
;; and indicate how many digits of precision are needed;
;; if they are not enough to fit the digits of the number the precision-loss-behavior is invoked.
;; The boolean false (the default) means the digits are the absolute position of the desired
;; smallest digit: negative means that the digits will represent a fraction of a unit
;; (e.g. -2 for cents of a dollar), whereas positive means that the digits will correspond to
;; a multiple of the unit (e.g. 3 for thousands of dollars).
;; If they are not enough to fit the digits of the number the precision-loss-behavior is invoked.
;; : String Integer <- Decimal (OrFalse Integer) Bool
(def (digits<-decimal n (position #f) (relative? #f))
  (when (and position relative?)
    (assert! (plus? position)))
  (defvalues (all-digits denominator-power) (digits-exponent<-decimal n))
  (def digit-count (count-digits all-digits))
  (def decimal-mark-index (- digit-count denominator-power))
  (if position
    (if relative?
      (if (plus? (+ position decimal-mark-index))
        (let ((new-digit-count (min digit-count
                                    (+ (min position (+ (max 0 (- decimal-mark-index))
                                                        digit-count))
                                       (min 0 decimal-mark-index)))))
          (unless (= new-digit-count digit-count)
            (case (precision-loss-behavior)
              ((error) (raise (disallowed-loss-of-precision)))
              ((truncate) (set! all-digits (truncate all-digits (expt 10 (- digit-count new-digit-count)))))
              ((round) (let-values (((rounded-all-digits remainder)
                                     (round/ all-digits (expt 10 (- digit-count new-digit-count)))))
                         (unless (or (>= remainder 0)
                                     (= digit-count (count-digits (- all-digits remainder))))
                           (increment! decimal-mark-index))
                         (set! all-digits rounded-all-digits)))
              (else (error "Invalid precision-loss-behavior")))
            (when (zero? all-digits)
              (set! decimal-mark-index 0)))
          (values (digits<-integer all-digits) decimal-mark-index))
        (values "0" 0))
      (if (plus? (- (max 0 decimal-mark-index) position))
        (let ((new-digit-count (min digit-count
                                    (- decimal-mark-index position))))
          (unless (= new-digit-count digit-count)
            (case (precision-loss-behavior)
              ((error) (raise (disallowed-loss-of-precision)))
              ((truncate) (set! all-digits (truncate all-digits (expt 10 (- digit-count new-digit-count)))))
              ((round) (let-values (((rounded-all-digits remainder)
                                     (round/ all-digits (expt 10 (- digit-count new-digit-count)))))
                         (unless (or (<= 0 remainder)
                                     (= digit-count (count-digits (- all-digits remainder))))
                           (increment! decimal-mark-index))
                         (set! all-digits rounded-all-digits)))
              (else (error "Invalid precision-loss-behavior")))
            (when (zero? all-digits)
              (set! decimal-mark-index 0)))
          (values (digits<-integer all-digits) decimal-mark-index))
        (values "0" position)))
    (values (digits<-integer all-digits) decimal-mark-index)))

;; Takes
;; - a decimal number,
;; - a number of allowed characters in which to fit the representation, or false if no limit,
;; - a number of desired fractional digits, or false if no limit,
;; - a scale, power of ten by which to notionally multiply the number (or false),
;; Returns
;; - a string for the digits plus the decimal mark,
;; - a boolean true iff the number is fractional (in which case it starts with the decimal mark),
;; - a boolean true iff the number is integral (in which case the decimal mark is NOT included in the string)
;; : String Nat Bool Bool (OrFalse Nat) <- Decimal (OrFalse Nat) (OrFalse Nat) (OrFalse Integer) (OrFalse Nat)
(def (%string<-decimal x (width #f) (fdigits #f) (scale #f))
  (assert! (decimal? x))
  (set! x (abs x))
  (defvalues (digits mark-place)
    (if fdigits
      (digits<-decimal x (- (+ fdigits (or scale 0))))
      (if (and width (> width 1))
        (let-values (((wd we)
                      (digits<-decimal x
                                       (max 1
                                            (+ (1- width)
                                               (if (and scale (minus? scale))
                                                 scale 0)))
                                       #t))
                     ((fd fe)
                      (digits<-decimal x (- (or scale 0)))))
          (if (>= (string-length wd) (string-length fd))
            (values wd we)
            (values fd fe)))
        (digits<-decimal x))))
  (def e (+ mark-place (or scale 0)))
  (def l (string-length digits))
  (def int? (<= l e))
  (def string
    (call-with-output-string
     (lambda (port)
       (if (plus? e)
         (begin
           (display (substring digits 0 (min l e)) port)
           (write-n-chars (- e l) #\0 port)
           (unless int?
             (write-char (decimal-mark) port)
             (display (substring digits (min l e) l) port)
             (when fdigits
               (write-n-chars (- fdigits (- l (min l e))) #\0 port))))
         (begin
           (write-char (decimal-mark) port)
           (write-n-chars (- e) #\0 port)
           (display digits port)
           (when fdigits
             (write-n-chars (+ fdigits e (- l)) #\0 port)))))))
  (values string (string-length string)
          (<= e 0)
          int?))

;; Port, Decimal,
;; a desired width in which to fit the number (or false),
;; a requested number of fractional digits (or false),
;; a scale (or false),
;; a character with which to fill the string to given width on overflow (or false),
;; a character to print when padding for desired width (or false if no desired width),
;; Return true iff there was an overflow and the overflow character was used.
;; : Bool <- Decimal Port \
;;   width:(OrFalse Nat) \
;;   integral-digits:(OrFalse Nat) \
;;   fractional-digits:(OrFalse Nat) \
;;   scale:(Or Integer Bool) \
;;   overflow:(OrFalse Char) \
;;   pad:Char \
;;   leading-decimal-mark-allowed?:Bool \
;;   always-decimal?:(Or Bool String) \
;;   always-sign?:Bool
(def (write-decimal number (port (current-output-port))
                    width: (w #f) fractional-digits: (d #f) scale: (k #f)
                    overflow: (ovf_ #f) pad: (pad_ #f)
                    leading-decimal-mark-allowed?: (leading-decimal-mark-allowed? #f)
                    always-decimal?: (always-decimal?_ #f) always-sign?: (always-sign? #f))
  (def pad (or pad_ #\space))
  (def ovf (if (eq? #t ovf_) #\X ovf_))
  (def always-decimal? (if (eq? #t always-decimal?_) (make-string 1 (decimal-mark)) always-decimal?_))
  (def spaceleft w)
  (when (and w (or always-sign? (> 0 number)))
    (decrement! spaceleft))
  (defvalues (str len frac? int?)
    (%string<-decimal (abs number) spaceleft d k))
  (when w
    (decrement! spaceleft len)
    ;; optional leading zero
    (when (and frac? (not leading-decimal-mark-allowed?))
      (decrement! spaceleft))
    ;; optional trailing decimal mark
    (when (and int? always-decimal?)
      (decrement! spaceleft (string-length always-decimal?))))
  (cond ((and w (minus? spaceleft) ovf)
         ;; field width overflow
         (write-n-chars w ovf port)
         #t)
        (else
         (when w
           (write-n-chars spaceleft pad port))
         (if (> 0 number)
           (write-char #\- port)
           (when always-sign?
             (write-char #\+ port)))
         (when (and frac? (not leading-decimal-mark-allowed?))
           (write-char #\0 port))
         (display str port)
         (when (and int? always-decimal?)
           (display always-decimal? port))
         #f)))

;; Takes:
;; - Decimal number
;; - Port
;; - a desired width in which to fit the number (or false),
;; - a requested number of fractional digits (or false),
;; - a scale, power of ten by which to notionally multiply the number (or false),
;; - a character to fill the string to desired width with on overflow (or false),
;; - a character to print when padding for desired width (or false if no desired width),
;; - a boolean (default false) true if the sign must always be printed even when positive.
;; Returns a string, or raise the symbol 'overflow if there was an overflow.
;; : String <- Decimal Port \
;;   width:(OrFalse Nat) \
;;   fractional-digits:(OrFalse Nat) \
;;   scale:(Or Integer Bool) \
;;   overflow:(OrFalse Char) \
;;   pad:(OrFalse Char) \
;;   always-decimal?:Bool \
;;   always-sign?:Bool
(def (string<-decimal number
                      width: (w #f) fractional-digits: (d #f) scale: (k #f)
                      overflow: (ovf #f) pad: (pad #f)
                      leading-decimal-mark-allowed?: (leading-decimal-mark-allowed? #f)
                      always-decimal?: (always-decimal? #f) always-sign?: (always-sign? #f))
  (call-with-output-string
   (lambda (port)
     (when (write-decimal number port width: w fractional-digits: d scale: k
                          overflow: ovf pad: pad
                          leading-decimal-mark-allowed?: leading-decimal-mark-allowed?
                          always-decimal?: always-decimal? always-sign?: always-sign?)
       (raise 'overflow)))))

;; Takes:
;; - Decimal number
;; - Port
;; - a desired width in which to fit the number (or false),
;; - a requested number of fractional digits (or false),
;; - a scale, power of ten by which to notionally multiply the number (or false),
;; - a character to fill the string to desired width with on overflow (or false),
;; - a character to print when padding for desired width (or false if no desired width),
;; - a boolean (default false) true if the sign must always be printed even when positive,
;; - a boolean (default false) true if the sign must always be printed even when positive.
;; Return a boolean true iff there was an overflow and the overflow character was used.
;; : Bool <- Decimal Port \
;;   width:(OrFalse Nat) \
;;   fractional-digits:(OrFalse Nat) \
;;   scale:(Or Integer Bool) \
;;   overflow:(OrFalse Char) \
;;   pad:(OrFalse Char) \
;;   always-decimal?:Bool \
;;   always-sign?:Bool
(def (write-amount decimal (port (current-output-port)) width:(w #f) integral-digits:(n #f) fractional-digits:(d #f) pad:(pad #\space) always-sign?:(always-sign? #f) sign-before-pad?:(sign-before-pad? #f))
  (assert! (decimal? decimal))
  (set! d (or d 2))
  (set! n (or n 1))
  (set! w (or w 0))
  (def signstr (cond ((> 0 decimal) "-")
                     (always-sign? "+")
                     (else "")))
  (def signlen (string-length signstr))
  (defvalues (str _strlen _ig2 _ig3) (%string<-decimal decimal #f d #f))
  (def pointplace (string-index str (decimal-mark)))
  (when sign-before-pad?
    (display signstr port))
  (write-n-chars (- w signlen (max n pointplace) 1 d) pad port)
  (unless sign-before-pad?
    (display signstr port))
  (write-n-chars (- n pointplace) #\0 port)
  (display str port))
