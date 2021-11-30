;; Decoding CSV, with byte buffers instead of character strings
;; Adapted from :std/text/csv
;; Problem: read-byte and Gerbil's peek-u8 are not quite super-optimized like Gambit's
;; read-char and peek-char, so this is actually 7x slower than the character-based version.
;; TODO: see Bradley Lucier's CSV code, initially from Phil Bewig:
;; https://gist.github.com/gambiteer/06fd167594763a095c3e628bfbd37161
;; https://mailman.iro.umontreal.ca/pipermail/gambit-list/2007-February/thread.html
;; Maybe generate an ad-hoc optimized parser for each family of parameters?
;; Maybe do our own buffering?


;; CSV is intrinsically an underspecified lossy format,
;; and even popular PC applications lose heavily
;; (i.e. no quoting convention at all, not even a pascal-like one)
;; when text fields contain the quote character. Ouch.
;;
;; This spec seems to explain popular usage, is refered by docs below.
;; http://www.creativyst.com/Doc/Articles/CSV/CSV01.htm
;;
;; This one says about the same:
;; http://edoceo.com/utilitas/csv-file-format
;;
;; There's now an RFC that tries to standardize CSV:
;; http://www.rfc-editor.org/rfc/rfc4180.txt
;;
;; Here's what Perl hackers think CSV is:
;; http://search.cpan.org/~hmbrand/Text-CSV_XS-0.59/CSV_XS.pm
;;
;;        (read-bcsv-file "foo.csv")
;;        (read-bcsv-lines port)
;;        (read-bcsv-line port)
;;        (write-bcsv-lines lines port)
;;        (write-bcsv-line fields port)
;;

(export
  current-bcsv-options bcsv-options make-bcsv-options
  bcsv-options-separator bcsv-options-separator-set!
  bcsv-options-quote bcsv-options-quote-set!
  bcsv-options-unquoted-quotequote? bcsv-options-unquoted-quotequote?-set!
  bcsv-options-loose-quote? bcsv-options-loose-quote?-set!
  bcsv-options-allow-binary? bcsv-options-allow-binary?-set!
  bcsv-options-eol bcsv-options-eol-set!
  bcsv-options-accept-lf? bcsv-options-accept-lf?-set!
  bcsv-options-accept-cr? bcsv-options-accept-cr?-set!
  bcsv-options-accept-crlf? bcsv-options-accept-crlf?-set!
  bcsv-options-skip-whitespace? bcsv-options-skip-whitespace?-set!
  creativyst-bcsv-options rfc4180-bcsv-options strict-rfc4180-bcsv-options
  call-with-creativyst-bcsv-syntax call-with-rfc4180-bcsv-syntax call-with-strict-rfc4180-bcsv-syntax
  with-creativyst-bcsv-syntax with-rfc4180-bcsv-syntax with-strict-rfc4180-bcsv-syntax
  read-bcsv-line read-bcsv-lines read-bcsv-file write-bcsv-line write-bcsv-lines write-bcsv-file
  )

(import
  (for-syntax :std/stxutil)
  :gerbil/gambit/bytes
  :scheme/base-ports
  :std/assert :std/error :std/iter :std/misc/list :std/srfi/1 :std/srfi/43 :std/stxparam :std/sugar
  ./bytestring)

; -----------------------------------------------------------------------------
;;; Parameters
(defstruct bcsv-options
  (separator ;; byte ;; Separator between CSV fields, usually #\, as in the name; sometimes #\tab.
   quote ;; byte ;; delimiter of string data; pascal-like quoted as double itself in a string.
   unquoted-quotequote? ;; bool ;; does a pair of quotes represent a quote outside of quotes?
                                ;; M$, RFC says no, csv.3tcl says yes
   loose-quote? ;; bool ;; can quotes appear anywhere in a field?
   allow-binary? ;; bool ;; do we accept non-ascii data?
   eol ;; string ;; what eol value do we output?
   accept-lf? ;; bool ;; is lf valid line-ending on input?
   accept-cr? ;; bool ;; is cr valid line-ending on input?
   accept-crlf? ;; bool ;; is crlf valid line-ending on input?
   skip-whitespace?) ;; bool ;; shall we skip unquoted whitespace around separators?
  transparent: #t)

(def rfc4180-bcsv-options
  (make-bcsv-options (b #\,) (b #\") #f #f #t +blf+ #t #f #t #f))
(def strict-rfc4180-bcsv-options
  (make-bcsv-options (b #\,) (b #\") #f #f #f +bcrlf+ #f #f #t #f))
(def creativyst-bcsv-options
  (make-bcsv-options (b #\,) (b #\") #f #f #t +bcrlf+ #t #t #t #t))

(def current-bcsv-options (make-parameter creativyst-bcsv-options))

(def (call-with-rfc4180-bcsv-syntax thunk)
  (parameterize ((current-bcsv-options rfc4180-bcsv-options)) (thunk)))
(defrule (with-rfc4180-bcsv-syntax () body ...)
  (call-with-rfc4180-bcsv-syntax (lambda () body ...)))

(def (call-with-strict-rfc4180-bcsv-syntax thunk)
  (parameterize ((current-bcsv-options strict-rfc4180-bcsv-options)) (thunk)))
(defrule (with-strict-rfc4180-bcsv-syntax () body ...)
  (call-with-strict-rfc4180-bcsv-syntax (lambda () body ...)))

(def (call-with-creativyst-bcsv-syntax thunk)
  (parameterize ((current-bcsv-options creativyst-bcsv-options)) (thunk)))
(defrule (with-creativyst-bcsv-syntax () body ...)
  (call-with-creativyst-bcsv-syntax (lambda () body ...)))

(def (valid-eol? x)
  (vector-member x +bline-endings+))

;; Only for inside this file: quick access to CSV options from variable bcsv-options.
(defsyntax-parameter* ambient-bcsv-options bcsv-options-param "Bad syntax; not in def/opt context")
(defsyntax (def/opt stx)
  (syntax-case stx ()
    ((d (f args ...) body ...)
     #'(def (f args ... (bcsv-options (current-bcsv-options)))
         (syntax-parameterize ((bcsv-options-param (quote-syntax bcsv-options)))
           body ...)))))
(defsyntax {stx}
  (syntax-case stx ()
    ((_ v) [(format-id #'v "bcsv-options-~a" #'v) #'ambient-bcsv-options])))
(defsyntax (w/opt stx)
  (syntax-case stx ()
    ((w f args ...) #'(f args ... ambient-bcsv-options))))

(def/opt (validate-bcsv-options)
  (assert! (byte? {separator}))
  (assert! (byte? {quote}))
  (assert! (not (equal? {separator} {quote})))
  (assert! (boolean? {unquoted-quotequote?}))
  (assert! (boolean? {loose-quote?}))
  (assert! (boolean? {skip-whitespace?}))
  (assert! (valid-eol? {eol}))
  (assert! (not (member (bytes-ref {eol} 0) [{separator} {quote}])))
  (assert! (boolean? {accept-lf?}))
  (assert! (boolean? {accept-cr?}))
  (assert! (boolean? {accept-crlf?}))
  (assert! (boolean? {skip-whitespace?})))

; -----------------------------------------------------------------------------
;;; The parser

(def (byte-ascii-text? b)
  (or (<= #x20 b #x7E) (= b 10) (= b 13)))

(def (accept pred port)
  (and (pred (peek-u8 port)) (read-byte port)))

(def (accept= c port)
  (and (eqv? c (peek-u8 port)) (read-byte port)))

(def (accept-eof port)
  (accept= #!eof port))

;; Read one line from PORT in CSV format, using the current syntax parameters.
;; Return a list of strings, one for each field in the line.
;; Entries are read as strings;
;; it is up to you to interpret the strings as whatever you want.
(def/opt (read-bcsv-line port)
  (def ss (open-output-u8vector))
  (def fields '())
  (def had-quotes? #f)
  (def (accept-eol port)
    (or (and {accept-lf?} (accept= 10 port) #t)
        (and (or {accept-cr?} {accept-crlf?})
             (and (accept= 13 port)
                  (or (and {accept-crlf?} (accept= 10 port))
                      {accept-cr?}
                      (raise-io-error 'accept-eol "Carriage-Return without Linefeed!"))))))
  ;; Is byte c some kind of white space?
  ;; NB: this only handles a tiny subset of whitespace characters, even if restricted to ASCII.
  ;; However, it's rather portable, and it is what the creativyst document specifies.
  ;; Be careful to not skip a separator, as it could be e.g. a tab!
  (def (byte-bcsv-space? c)
    (and (or (eqv? c #x20) (eqv? c #x09)) (not (eqv? c {separator}))))
  (def (accept-space port)
    (accept byte-bcsv-space? port))
  (def (accept-spaces port)
    (with-list-builder (c) (let loop () (let ((x (accept-space port))) (when x (c x) (loop))))))
  (def (accept-fields)
    (set! had-quotes? #f)
    (when {skip-whitespace?} (accept-spaces port))
    (if (and (null? fields) (or (accept-eol port) (accept-eof port)))
      (done)
      (accept-field-start)))
  (def (accept-field-start)
    (cond
     ((accept= {separator} port)
      (add "") (accept-fields))
     ((accept= {quote} port)
      (cond
       ((and {unquoted-quotequote?} (accept= {quote} port))
        (add-byte {quote})
        (accept-field-unquoted))
       (else
        (accept-field-quoted))))
     (else
      (accept-field-unquoted))))
  (def (accept-field-quoted)
    (set! had-quotes? #t)
    (cond
     ((accept-eof port)
      (raise-io-error 'read-bcsv-line "unexpected eof in quotes"))
     ((accept= {quote} port)
      (cond
       ((accept= {quote} port)
        (quoted-field-byte {quote}))
       ({loose-quote?}
        (accept-field-unquoted))
       (else
        (add (current-bytes))
        (end-of-field))))
     (else
      (quoted-field-byte (read-byte port)))))
  (def (quoted-field-byte c)
    (add-byte c)
    (accept-field-quoted))
  (def (accept-field-unquoted)
    (if {skip-whitespace?}
      (let ((spaces (accept-spaces port)))
        (cond
         ((accept= {separator} port)
          (add (current-bytes))
          (accept-fields))
         ((or (accept-eol port) (accept-eof port))
          (add (current-bytes))
          (done))
         (else
          (for-each add-byte spaces)
          (accept-field-unquoted-no-skip))))
      (accept-field-unquoted-no-skip)))
  (def (accept-field-unquoted-no-skip)
    (cond
     ((accept= {separator} port)
      (add (current-bytes))
      (accept-fields))
     ((or (accept-eol port) (accept-eof port))
      (add (current-bytes))
      (done))
     ((accept= {quote} port)
      (cond
       ((and {unquoted-quotequote?} (accept= {quote} port))
        (add-byte {quote}) (accept-field-unquoted))
       ({loose-quote?}
        (accept-field-quoted))
       (else
        (raise-io-error 'read-bcsv-line "unexpected quote in middle of field"))))
     (else
      (add-byte (read-byte port))
      (accept-field-unquoted))))
  (def (end-of-field)
    (when {skip-whitespace?} (accept-spaces port))
    (cond
     ((or (accept-eol port) (accept-eof port))
      (done))
     ((accept= {separator} port)
      (accept-fields))
     (else
      (raise-io-error 'read-bcsv-line "end of field expected"))))
  (def (add x)
    (push! x fields))
  (def (add-byte c)
    (unless (or {allow-binary?} (byte-ascii-text? c))
      (raise-io-error 'read-bcsv-line "binary data not allowed" c))
    (write-byte c ss))
  (def (current-bytes)
    (get-output-u8vector ss))
  (def (done)
    (reverse! fields))
  (accept-fields))

;; Read lines from PORT in CSV format, using the current syntax parameters.
;; Return a list of list of strings, one entry for each line,
;; that contains one entry for each field.
;; Entries are read as strings;
;; it is up to you to interpret the strings as whatever you want.
(def/opt (read-bcsv-lines port)
  (w/opt validate-bcsv-options)
  (call-with-list-builder (lambda (c _) (until (accept-eof port) (c (w/opt read-bcsv-line port))))))

;; Open the file designated by the path, using the provided settings if any,
;; and call read-bcsv-lines on it.
(def/opt (read-bcsv-file path-or-settings)
  (call-with-input-file path-or-settings (cut w/opt read-bcsv-lines <>)))

(def/opt (byte-needs-quoting? x)
  (or (eqv? x {quote})
      (eqv? x {separator})
      (eqv? x 13)
      (eqv? x 10)
      (not (byte-ascii-text? x))))

;; Is byte b some kind of white space?
;; NB: this only handles a tiny subset of whitespace characters, even if restricted to ASCII.
;; However, it's rather portable, and it is what the creativyst document specifies.
;; Be careful to not skip a separator, as it could be e.g. a tab!
(def/opt (byte-bcsv-space? b)
  (and (or (eqv? b #x20) (eqv? b #x09)) (not (eqv? b {separator}))))

(def/opt (bytes-needs-quoting? x)
  (and (< 0 (bytes-length x))
       (or (w/opt byte-bcsv-space? (bytes-ref x 0))
           (w/opt byte-bcsv-space? (bytes-ref x (1- (bytes-length x))))
           (vector-index byte-needs-quoting? x))
       #t))

;; Given a list of LINES, each of them a list of fields, and a PORT,
;; format those lines as CSV according to the current syntax parameters.
(def/opt (write-bcsv-lines lines port)
  (for-each (cut w/opt write-bcsv-line <> port) lines))

;; Writes list of LINES to the designated PATH using write-bcsv-lines
;; and the provided settings.
(def/opt (write-bcsv-file path-or-settings lines)
  (call-with-output-file path-or-settings
    (cut w/opt write-bcsv-lines lines <>)))

;; Format one line of FIELDS to PORT in CSV format,
;; using the current syntax parameters.
(def/opt (write-bcsv-line fields port)
  (let loop ((fields fields))
    (match fields
      ([first . rest]
       (write-bcsv-field first port)
       (unless (null? rest)
         (write-byte {separator} port))
       (loop rest))
      ([] (display {eol} port)))))

(def/opt (write-bcsv-field field port)
  (match field
    ((? not) (void))
    ((? number?) (display field port))
    ((? bytes?) (w/opt write-bcsv-bytes-safely field port))
    ((? string?) (w/opt write-bcsv-bytes-safely (b field) port))
    ((? symbol?) (w/opt write-bcsv-bytes-safely (b (symbol->string field)) port))
    (else (error "invalid CSV field" field))))

(def/opt (write-bcsv-bytes-safely bytes port)
  (if (bytes-needs-quoting? bytes)
    (w/opt write-quoted-bytes bytes port)
    (write-bytes bytes port)))

(def/opt (write-quoted-bytes bytes port)
  (write-byte {quote} port)
  (for (i (in-range 0 (bytes-length bytes)))
    (when (= c {quote})
      (write-byte c port))
    (write-byte c port))
  (write-byte {quote} port))
