;; -*- Gerbil -*-
(export #t)

(import
  :gerbil/gambit/ports
  :std/iter :std/misc/number :std/srfi/13
  ./basic-parsers ./list)

;; TODO: write a string-substitute function in the style of http://clhs.lisp.se/Body/f_sbs_s.htm
;; and/or of SRFI 13 (that will be contributed to std/misc/string);
;; and/or port and contribute to Gerbil an existing SRFI that has such a function already if any.
;; String <- Char Char String from-end: ? Bool start: ? (OrFalse Fixnum) end: ? (OrFalse Fixnum) count: (OrFalse Fixnum)
(def (string-substitute new-char old-char string
                        from-end: (from-end #f)
                        start: (start #f)
                        end: (end #f)
                        count: (count #f))
  (let* ((l (string-length string))
         (new-string (make-string l))
         (k 0))
    (for ((i (in-iota l)))
      (let ((char (string-ref string i)))
        (string-set! new-string i (if (and (eqv? char old-char)
                                           (or (not start) (<= start i))
                                           (or (not end) (< i end))
                                           (or (not count) (< k count)))
                                    (begin (increment! k) new-char)
                                    char))))
    new-string))

;; Given a string, return it with any beginning or ending whitespace trimmed off
;; String <- String
(def (string-trim-spaces string)
  (string-trim-both string char-ascii-whitespace?))

;; Given a list of strings, return a list of the same strings where those
;; shorter than the longest one have been right-padded with spaces.
;; (List String) <- (List String)
(def (co-pad-strings strings)
  (def maxlen (extremum<-list > (map string-length strings) 0))
  (map (cut string-pad-right <> maxlen) strings))

;; Given a string and a function that interpolates strings inside ${xxx} into strings,
;; replace the ${strings} with the function results, and any $$ with single $.
;; Note that in this simple interpolation function, parsing is not recursive:
;; once we find "${" we look for the first matching "}" and do not try to parse the contents.
;; This function is therefore not adequate for use as part of e.g. a full-fledged shell
;; command-line parser, or more generally when function f itself parses expressions
;; that may contain #\} characters. But it is good enough when the parameter f is a simple
;; variable lookup, or has very simple expressions in which character #\} isn't allowed.
;; String <- String (Fun String <- String)
(def (string-interpolate s f)
  (def l (string-length s))
  (def (c i) (and (> l i) (string-ref s i)))
  (call-with-output-string
    (lambda (o)
      (let loop ((i 0))
        (def j (string-index s #\$ i))
        (if j
          (begin
            (when (> j i) (write-substring s i j o))
            (let (ch (c (1+ j)))
              (case ch
                ((#\{)
                 (let (k (string-index s #\} (+ j 2)))
                   (unless k
                     (error "Invalid interpolation" s j))
                   (write-string (f (substring s (+ j 2) k)) o)
                   (loop (1+ k))))
                ((#\$) (write-char ch o) (loop (+ j 2)))
                (else (write-char #\$ o) (loop (+ j 1))))))
          (write-substring s i l o))))))
