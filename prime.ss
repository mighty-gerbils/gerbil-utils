;; Dealing with small prime numbers (say up to 1e8, if using a few GiB of memory)
;; NB: This code maintains global tables and is generally not thread-safe.
;; See also "The Genuine Sieve of Erathostenes" by Melissa E. O'Neill:
;;      https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/exact :gerbil/gambit/misc :gerbil/gambit/random
  :scheme/base-impl
  :std/iter :std/misc/list-builder :std/misc/number :std/srfi/1 :std/sugar
  ./number ./extensible-vector)

;; Given a list of `primes`, return a vector the size of which is the product M of those primes,
;; that at index I contains the smallest positive increment J such that I+J is a unit modulo M.
;; This makes it easier to skip over obvious composite numbers when looking for primes.
(def (compute-prime-wheel primes)
  (def product (reduce * 1 primes))
  (assert! (and (fixnum? product) (<= 2 product)))
  (def rp (list->vector (for/collect (n (in-range product)) (= 1 (gcd n product)))))
  (list->vector (for/collect (n (in-range product))
                  (let/cc return
                    (for (i (in-range 1 product))
                      (when (vector-ref rp (modulo (+ n i) product)) (return i)))))))

;; A wheel for skipping over numbers that are divisible by small primes. See `compute-prime-wheel`.
;; Note that the these numbers MUST already be in `primes`, or else the sieve will fail.
(def wheel (compute-prime-wheel '(2 3 5 7)))

;; Compute the position of a NUMBER in the WHEEL. See COMPUTE-PRIME-WHEEL.
(def (wheel-position wheel number)
  (modulo number (vector-length wheel)))

;; Given a NUMBER at given POSITION in the WHEEL, return the next NUMBER that is not divisible
;; by a factor of the wheel size, and its position in the wheel.
(def (wheel-next wheel number (position (wheel-position wheel number)))
  (let ((increment (vector-ref wheel position)))
    (values (+ number increment) (modulo (+ position increment) (vector-length wheel)))))

;; NB: 0 is so the useful array indices start with 1
(def primes (evector<-list '(0 2 3 5 7)))

;; Given an integer N, return the Nth prime number, starting with prime number 2 at index 1
(def nth-prime (auto-evector cache: primes fun: (lambda (n) (next-prime-above (nth-prime (1- n))))))

;; sieve: 1 if the number is prime, 0 if composite. Must be initialized to 1 beyond the fill-pointer.
(def prime-sieve (make-ebits (list->u8vector '(#xAC #xF8)) 11))

;; Return the next prime number P such that P > N
(def (next-prime-above n)
  ;;(assert! (and (exact-integer? n) (plus? n)))
  (let loop ((k n) (wp (wheel-position wheel n)))
    (defvalues (l p) (wheel-next wheel k wp))
    (if (prime? l) l (loop l p))))

;; Given a small integer N, is it a prime number?
;; Answer using Erathostenes' sieve.
;; Note: the sieve is not thread-safe.
(def (prime?/sieve n)
  (let/cc return
    (when (< n (ebits-fill-pointer prime-sieve))
      (return (ebits-set? prime-sieve n)))
    (def fp (evector-fill-pointer primes))
    (for (i (in-range 1 fp))
      (let (p (evector-ref primes i))
        (when (< n (* p p)) (return #t))
        (when (zero? (modulo n p)) (return #f))))
    (erathostenes-sieve (integer-sqrt n))
    (for (i (in-range fp (evector-fill-pointer primes)))
      (when (zero? (modulo n (evector-ref primes i))) (return #f)))
    #t))

;; The largest small prime computed so far
(def (largest-known-prime)
  (evector-ref primes (1- (evector-fill-pointer primes))))

;; Run the sieve of Erathostenes up to `n`
(def (erathostenes-sieve n)
  (def m (ebits-fill-pointer prime-sieve))
  (when (>= n m)
    (let (r (integer-sqrt n))
      (extend-ebits! prime-sieve (1+ n) initial-value: 1)
      (set! (ebits-fill-pointer prime-sieve) (1+ n))
      (def (sieve! p)
        (def p2 (* p p))
        (let loop ((q (if (>= p2 m) p2 (ceiling-align m p))))
          (when (<= q n)
            (ebits-set! prime-sieve q 0)
            (loop (+ q p)))))
      (let/cc return
        (for (i (in-range 1 (evector-fill-pointer primes)))
          (let (p (nth-prime i))
            (when (> p r) (return))
            (sieve! p)))
        (def p (largest-known-prime))
        (let loop ((p p) (wp (wheel-position wheel p)))
          (defvalues (p1 wp1) (wheel-next wheel p wp))
          (when (> p1 n) (return))
          (when (ebits-set? prime-sieve p1)
            (evector-push! primes p1 extend: #t)
            (sieve! p1))
          (loop p1 wp1)))
      (let ((p (largest-known-prime)))
        (when (< p n)
          (let loop ((p p) (wp (wheel-position wheel p)))
            (defvalues (p1 wp1) (wheel-next wheel p wp))
            (unless (> p1 n)
              (when (ebits-set? prime-sieve p1)
                (evector-push! primes p1 extend: #t))
              (loop p1 wp1))))))))


;; (pi-function n) is the number of positive prime integers no greater than n
(def pi-cache (evector<-list '(0 0 1 2 2 3 3 4 4 4 4)))
(def pi-function
  (auto-evector
   cache: pi-cache fun:
   (lambda (n)
     (def m (evector-fill-pointer pi-cache))
     (def sp (evector-ref pi-cache (1- m)))
     (extend-evector! pi-cache (1+ n))
     (set! (evector-fill-pointer pi-cache) (1+ n))
     (erathostenes-sieve n)
     (for (m (in-range m (1+ n)))
       (set! (evector-ref pi-cache m) (pre-increment! sp (ebits-ref prime-sieve m))))
     sp)))

;; Does `f` divide `n`?
(def (divides? f n)
  (assert! (nat? f))
  (assert! (nat? n))
  (if (zero? f)
      (zero? n)
      (zero? (modulo n f))))

;; Given an integer N, return a non-decreasing list of its prime factors, using the sieve
(def (factor n)
  (assert! (and (nat? n) (plus? n)))
  (with-list-builder (f)
    (let loop ((i 1) ;; index of the next prime to try
               (n n) ;; product of remaining factors
               (max #f)) ;; biggest number we have to test before we can be sure n is prime
      (when (< 1 n)
        (let* ((p (nth-prime i))
               (max (or max (integer-sqrt n)))) ;; integer-sqrt is expensive, so recompute the limit lazily
          (if (> p max) (f n)
              (let loop2 ((n n) (max max)) ;; divide by this prime as many times as possible
                (let-values (((m r) (floor/ n p)))
                  (if (zero? r)
                    (begin (f p) (loop2 m #f))
                    (loop (1+ i) n max))))))))))

;; Given integers `a` and `n`, and ancillary data,
;; is `a` a witness of n's compositeness as per the Miller test?
(def (witness-of-compositeness? a n n-1 r d)
  ;; (assert! (every nat? [a n n-1 r d]))
  ;; (assert! (= (1- n) n-1))
  ;; (assert! (= n-1 (* (expt 2 r) d)))
  ;; (assert! (odd? d))
  (def x (expt-mod a d n))
  (let/cc return
    (when (or (= x 1) (= x n-1))
      (return #f)) ;; not a witness
    (for (_ (in-range (1- r)))
      (set! x (expt-mod x 2 n))
      (when (= x 1) (return #t)) ;; witness that n is composite
      (when (= x n-1) (return #f))) ;; not a witness
    #t)) ;; witness that n is composite

;; How many times does 2 divide N? Return -1 for 0.
(def (valuation-of-2 n)
  (1- (integer-length (bitwise-xor n (1- n)))))

;; Is integer N a prime number? Use Miller method to check,
;; with a list of candidate witnesses AS.
(def (prime?/miller n as)
  (let* ((n-1 (- n 1))
         (r (valuation-of-2 n-1))
         (d (arithmetic-shift n-1 (- r))))
    (every (cut witness-of-compositeness? <> n n-1 r d) as)))

;; Is integer `n` a prime number? Use Rabin-Miller method to check.
(def (prime?/miller-rabin n)
  (let* ((n-1 (- n 1))
         (r (valuation-of-2 n-1))
         (d (arithmetic-shift n-1 (- r))))
    ;; Each independent test reduces the probability of primality by 1/4
    ;; We add a constant 4^16 = 2^64 error factor.
    (let/cc return
      (for (_ (in-range (- 16 (floor-quotient (- (integer-length n)) 4))))
        (let (a (+ 2 (random-integer (- n 3))))
          (when (witness-of-compositeness? a n n-1 r d) (return #f))))
      #t)))

;; Is natural integer `n` a prime number?
(def (prime? n)
  (assert! (nat? n))
  (cond
    ((< n 2) #f)
    ((< n 1000000) (prime?/sieve n))
    ((< n 3317044064679887385961981) ;; NB: this number's integer-length is 82
     (prime?/miller n '(2 3 5 7 11 13 17 19 23 29 31 37 41)))
    (else (prime?/miller-rabin n))))
