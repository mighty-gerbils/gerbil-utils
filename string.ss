;; -*- Gerbil -*-
(export #t)

(import
  :std/iter :std/misc/number :std/srfi/13)

;; TODO: write a string-substitute function in the style of http://clhs.lisp.se/Body/f_sbs_s.htm
;; and/or of SRFI 13 (that will be contributed to std/misc/string);
;; and/or port and contribute to Gerbil an existing SRFI that has such a function already if any.
;; Then use it below in truefx/collector#truefx-logger instead of pregexp-replace (ugh)?
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
