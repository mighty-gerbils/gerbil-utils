;; -*- Gerbil -*-
;;;; Utilities to generate random data

(export #t)

(import
  (only-in :gerbil/gambit random-source-randomize! default-random-source)
  (only-in :std/misc/list-builder with-list-builder)
  (only-in :std/misc/number decrement! uint-length-in-u8)
  (only-in :std/misc/bytes u8vector->uint)
  (only-in :std/crypto/etc random-bytes)
  (only-in ./base nest λ))

;; cryptographically random integers
(def (random-uint end)
  ;; Instead of skew or retries from (uint-length-in-u8 (1- end)) bytes,
  ;; take 8 more bytes and consider the skew negligible.
  (def n (u8vector->uint (random-bytes (+ (uint-length-in-u8 end) 7))))
  (modulo n end))

(def (random-char bag)
  (string-ref bag (random-uint (string-length bag))))
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

(def (randomize! (random-source default-random-source))
  (random-source-randomize! random-source))

(def (shuffle-list list (n #f))
  (nest
   (let* ((vec (list->vector list))
          (len (vector-length vec))))
   (with-list-builder (collect _))
   (let loop ((end (if n (min n len) len))))
   (when (< 0 end))
   (let* ((i (random-uint end))
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
      (loop (+ sum 1 (random-uint sides)))))))
