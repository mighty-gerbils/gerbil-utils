;; Scribble: Racket-like scribble reader extension for Gerbil Scheme -*- Scheme -*-
;; WIP -- gotta figure out how to integrate into Gambit / Gerbil 's reader API,
;; which differs from CL. Also gotta figure out the column situation.

;; See Racket documentation: http://docs.racket-lang.org/scribble/reader.html
;; And racket source code: pkgs/at-exp-lib/scribble/reader.rkt

(export #t)
(import
  :std/io
  (only-in :std/srfi/13 string-index)
  (only-in :std/text/char-set def-codepoint codepoint-ascii-alphanumeric?
           char-strict-whitespace?)
  (only-in :std/misc/number increment! decrement!)
  ./base)

(def-codepoint (ascii-punctuation? c)
  (and (<= 33 c 126) (not (codepoint-ascii-alphanumeric? c))))

#|
;; Parse an @ expression.
(def (parse-at-syntax input)
  (using (input :- BufferedInput)
    (def o (open-output-string)) ; buffered output of "current stuff"
    (def i (make-instance 'buffered-input :stream input))
    (def cmdonly #f)
    (def col 0)
    (def line '())
    (def lines '())
    (def mrof '()) ; current form (reversed)
    (def (expect pred) ((parse-maybe-one-of pred) input))
    (def (expect= c) (expect (cut eqv? <> c)))
    (def (expect-in string) (expect (cut string-index string <>)))
    ;; functions starting with ? process input after matching what is described in the name,
    ;; e.g. ?at processes input after an at-sign @.
    ;; those ending with ! issue output.
    (def (?at) ; what to do after a @
      (cond
       ((expect char-strict-whitespace?)
        (raise-parse-error parse-at-syntax "Unexpected whitespace after @" input))
       ((expect= #\;)
        (?at-comment))
       (else
        (?punctuation))))
    (def (?at-comment) ; what to do after @;
      (cond
       ((expect= #\{) (?brace-text))
       (else (read-line input)))
      (parse-and-skip-any-whitespace input))
    (def (?punctuation)
      (def char (expect-in "'`,"))
      (case char
        ((#\') (?quote))
        ((#\`) (?backquote))
        ((#\,) (cond
                ((expect= #\@) (?comma-at))
                ((expect= #\.) (?comma-dot))
                (else (?comma))))
        (else (?cmd))))
    (def (?quote)
      (kwote (?punctuation)))
    (def (?backquote)
      (call-with-quasiquote-reader ?punctuation))
    (def (?comma-at)
      (call-with-unquote-splicing-reader ?punctuation))
    (def (?comma-dot)
      (call-with-unquote-nsplicing-reader ?punctuation))
    (def (?comma)
      (call-with-unquote-reader ?punctuation))
    (def (?cmd)
      (def char (expect-in "|[{"))
      (case char
        ((#\|) (?maybe-alttext ?at-pipe))
        ((#\[ #\{) (?datatext char))
        (else (?cmd1))))
    (def (?maybe-alttext cont)
      (input.unread-char #\|)
      (def k (?newkey))
      (cond
       (k
        (set! cmdonly #f)
        (?brace-alttext k))
       (else
        (cont))))
    (def (?at-pipe)
      (expect= #\|) ;; re-read and drop the #\|
      (def r (expect-to-char #\| i))
      (define-values (s n) (read-from-string r))
      (unless (eof-object? (ignore-errors (read-from-string r #f #!eof :start n)))
        (raise-parse-error parse-at-syntax "Unexpected characters in ~S after position ~D" r n))
      (set! cmdonly #t)
      (form! s)
      (?end))
    (def (?cmd1)
      (set! cmdonly #t)
      (form! (parse-whitespace input))
      (?cmd2))
    (def (?cmd2)
      (def char (expect-in "[{|"))
      (if char
        (?datatext char)
        (?end)))
    (def (form! x)
      (push! x mrof))
    (def (?datatext char)
      (case char
        (#\[ (?square-bracket-data))
        ((#\{ #\|) (unread-char* i char) (?brace-text0))))
    (def (?square-bracket-data)
      (set! cmdonly #f)
      (for-each form! (read-delimited-list #\] input #t))
      (?brace-text0))
    (def (?brace-text0)
      (cond
       ((expect= #\{)
        (set! cmdonly #f)
        (?brace-text))
       ((expect= #\|)
        (?maybe-alttext ?end))
       (else (?end))))
    (def (?newkey)
      (expect= #\|)
      (let loop ((r '()))
        (def c (input.read-char))
        (cond
         ((and (char-ascii-punctuation? c) (not (string-index "@|{" c)))
          (push! c r) (loop))
         ((eqv? c #\{)
          (list->string (reverse r)))
         (else
          (input.put-back (reverse r))
          #f))))
    (def (char! c)
      (write-char c o))
    (def (flush!)
      (def s (get-output-string o))
      (when (plus? (length s))
        (push! s line)))
    (def (eol! eol)
      (def s0 (get-output-string o))
      (def s (if eol (trim-ending-spaces s0) s0))
      (when (plus? (length s))
        (push! s line))
      (push! (cons col (reverse line)) lines)
      (when eol
        (set! col (skip-whitespace-return-column i 0))
        (set! line '()))
      #t)
    (def (?brace-text)
      (def brace-level 1)
      (set! col (stream-line-column-harder input))
      (set! line '())
      (let loop ()
        (def c (input.read-char))
        (case c
          ((#\return)
           (expect= #\newline)
           (eol! #t)
           (loop))
          ((#\newline)
           (eol! #t)
           (loop))
          ((#\{)
           (increment! brace-level)
           (char! c)
           (loop))
          ((#\@)
           (?inside-at)
           (loop))
          ((#\})
           (decrement! brace-level)
           (cond
            ((zero? brace-level)
             (eol! #f)
             (flush-text!)
             (?end))
            (else
             (char! c)
             (loop))))
          (else
           (char! c)
           (loop)))))
    (def (?inside-at)
      (def c (expect-in ";\"|"))
      (case c
        ((#\;)
         (cond
          ((expect= #\{)
           (let ((m mrof) (l line) (ls lines) (c col) (co cmdonly) (oo o))
             (set! o (open-output-string))
             (?brace-text)
             (set! mrof m line l lines ls col c cmdonly co o oo)))
          (else
           (read-line input)
           (skip-whitespace-return-column input))))
        ((#\")
         (unread-char* i #\")
         (flush-buffer i)
         (write-string (read-preserving-whitespace input #t #f #f) o))
        ((#\|)
         (flush!)
         (let ((r (read-to-char #\| i)))
           (with-input-from-string (s r)
             (loop :for x = (read-preserving-whitespace s #f s #f)
                   :until (eq x s) :do (push! x line)))))
        (else
         (flush!)
         (flush-buffer i)
         (push! (parse-at-syntax input) line))))
    (def (flush-text!)
      (def mincol (loop :for (col . strings) :in lines
                        :when strings :minimize col))
      (def text (loop :for (col . strings) :in (reverse lines)
                      :for first = #t :then #f
                      :append
                      `(,@(when (and strings (> col mincol) (not first))
                            (list (n-spaces (- col mincol))))
                        ,@strings ,*lf*)))
      (when (eq *lf* (first text))
        (pop! text))
      (def e (every (lambda (x) (eq x *lf*)) text))
      (def r (reverse text))
      (unless e
        (loop :repeat 2 :when (eq *lf* (first r)) :do (pop r)))
      (set! mrof (append r mrof))
      #t)
    (def (?brace-alttext key)
      (def brace-level 1)
      (def rkey (mirror-string key))
      (input.flush-buffer)
      (set! col (stream-line-column-harder input))
      (set! line '())
      (let/cc return
        (while #t
          (def c (input.read-char))
          (case c
            ((#\return)
             (expect= #\newline)
             (eol! #t))
            ((#\newline)
             (eol! #t))
            ((#\|)
             (if (not (expect-string i key))
               (char! #\|)
               (let ((c (expect-in "@{")))
                 (case c
                   ((#\{)
                    (increment! brace-level)
                    (char! #\|)
                    (string-for-each char! key)
                    (char! c))
                   ((#\@)
                    (?inside-at))
                   (else
                    (input.put-back (string->list key))
                    (char! #\|))))))
              ((#\})
               (cond
                ((not (expect-string i rkey))
                 (char! #\}))
                ((expect= #\|)
                 (decrement! brace-level)
                 (cond
                  ((zero? brace-level)
                   (eol! #f)
                   (flush-text!)
                   (return (?end)))
                  (else
                   (char! #\})
                   (string-for-each char! rkey)
                   (char! #\|))))
                (else
                 (unread-string i rkey)
                 (char! #\}))))
              (else
               (char! c))))))
    (def (?end)
      (flush-buffer i)
      (if (and cmdonly (length=n? mrof 1))
        (car mrof)
        (reverse mrof)))
    (?at))) ;; a @ character was just read by who called this function, so start parsing at ?at

(defun read-at-syntax (stream &optional char)
  (declare (ignore char))
  (parse-at-syntax stream))
(defun forbidden-pipe-macro (stream char)
  ;; if we allow pipes, then @foo|{bar}| gets read as @ followed by escaped symbol |foo{bar}|
  ;; maybe we could make | a terminating macro and otherwise keep its meaning?
  (declare (ignore stream char))
  (simple-parse-error "| not allowed when at syntax enabled"))

(defun do-enable-scribble-at-syntax (&key (table *readtable*) scribe skribe)
  (enable-quasiquote :table table)
  (flet ((s (char fun) (set-macro-character char fun #f table)))
    (s #\[ read-paren-list)
    (s #\] unbalanced-paren)
    (s #\{ read-paren-list)
    (s #\} unbalanced-paren)
    (s #\@ read-at-syntax)
    (s #\| forbidden-pipe-macro)
    (when (or scribe skribe) ;; backward compatibility with former scribble?
      (do-enable-scribble-syntax table)))
  #t)

(defvar *scribble-at-readtable* #f)
(defun enable-scribble-at-syntax (&key (table *readtable*) (scribe #f))
  (set! *scribble-at-readtable* (push-readtable table))
  (do-enable-scribble-at-syntax :table *scribble-at-readtable* :scribe scribe)
  *scribble-at-readtable*)
(defun disable-scribble-at-syntax ()
  (pop-readtable))
(def (reenable-scribble-at-syntax scribe: (scribe #f))
  (if (readtable? *scribble-at-readtable*)
    (enable-scribble-at-syntax :scribe scribe)
    (push-readtable *scribble-at-readtable*))
  *scribble-at-readtable*)

(def (parse-at-string s)
  (def i (open-buffered-string-reader s))
  (parameterize ((*readtable* *scribble-at-readtable*))
    (parse-at-syntax i)))
|#
