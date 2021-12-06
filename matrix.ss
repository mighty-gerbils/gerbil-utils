;; Simple linear algebra

;; Storing matrixes with I columns and J lines, line-by-line,
;; so that you can view the source code as the matrix value.
;; NB: that's opposite from the row-major order of CL, more like column-major.

(export #t)

(import :gerbil/gambit/bits :std/iter)

;; Generic exponentiation. TODO: move to a better place?
(def (generic-expt composer function iterations seed)
  (if (zero? iterations) seed
      (generic-expt composer
		    (composer function function)
		    (arithmetic-shift iterations -1)
		    (if (odd? iterations)
			(composer function seed)
			seed))))

;; Accessing a IxJ matrix M's element at column i line j
(def (Mref I J M i j) (vector-ref M (+ i (* J j))))
(def (Mset! I J M i j v) (vector-set! M (+ i (* I j)) v))
(def Mref-set! Mset!)

;; zero matrix of any rectangular shape
(def (matrix0 I J) (make-vector (* I J) 0))

;; identity square matrix
(def (matrix1 I)
  (def M (matrix0 I I))
  (for (i (in-range I)) (Mset! I I M i i 1))
  M)

;; matrix addition for any shape
(def (matrix+ a b)
  (vector-map (lambda (_ x y) (+ x y)) a b))

;; matrix multiplication (composition) with shape I J K
(def (matrix* I J K A B)
  (def M (matrix0 I K))
  (for (k (in-range K))
    (for (i (in-range I))
      (Mset! I K M i k
             (for/fold (sum 0) (j (in-range J))
               (+ sum (* (Mref I J A i j) (Mref J K B j k)))))))
  M)

;; matrix exponentiation
(def (matrix-expt I A n)
  (generic-expt (cut matrix* I I I <> <>) A n (matrix1 I)))
