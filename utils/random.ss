;; -*- Gerbil -*-
;;;; Utilities to generate random data

(export
  #t)

(import
  :gerbil/gambit/random
  :std/misc/list :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/number)

(def (random-char bag)
  (string-ref bag (random-integer (string-length bag))))
(def (generate-list n generate-element)
  (if (zero? n) '() (cons (generate-element) (generate-list (- n 1) generate-element))))
(def (n-random-chars n bag)
  (list->string (generate-list n (λ () (random-char bag)))))

(def all-alphanumerics "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")
(def (six-alphanumerics) (n-random-chars 6 all-alphanumerics))

;; OKCoin password generator
;; TODO: seed the PRNG from /dev/urandom, triple-check what algorithm Marc uses.
(def (generate-OKCoin-password)
  (n-random-chars 32 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890&!?"))

(def (randomize! (random-source (default-random-source)))
  (random-source-randomize! random-source))

(def (shuffle-list list (n #f))
  (nest
   (let* ((vec (list->vector list))
          (len (vector-length vec))))
   (with-list-builder (collect _))
   (let loop ((end (if n (min n len) len))))
   (when (< 0 end))
   (let* ((i (random-integer end))
          (max (- end 1)))
     (collect (vector-ref vec i))
     (vector-set! vec i (vector-ref vec max))
     (loop max))))

(def (dice (n 1) (sides 6))
  (let loop ((sum 0))
    (cond
     ((zero? n) sum)
     (else
      (decrement! n)
      (loop (+ sum 1 (random-integer sides)))))))
