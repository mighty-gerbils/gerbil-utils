;; -*- Gerbil -*-
;;;; Basic printers

(export #t)

(import
  :scheme/base
  :std/error :std/srfi/13 :std/sugar
  :clan/utils/base)

;; Assume ASCII, base 2 to 36
;; : (Or Char '#f) <- Integer (Optional Integer 10) (Optional Bool #f)
(def (digit-char n (base 10) (upper-case? #f))
  (and (exact-integer? n) (exact-integer? base)
       (< -1 n base 37)
       (integer->char (+ n (cond
                            ((< n 10) 48) ;; ASCII 0-9
                            (upper-case? 55) ;; ASCII A-Z
                            (else 87)))))) ;; ASCII a-z

(def (string<-integer-base integer (base 10))
  (string-reverse
   (call-with-output-string
    []
    (λ (port)
      (if (= integer 0)
        (write-char #\0)
        (let loop ((n (abs integer)))
          (unless (zero? n)
            (let-values (((q r) (floor/ n base)))
              (write-char (digit-char r base) port)
              (loop q)))))
      (when (< integer 0)
        (write-char #\- port))))))

