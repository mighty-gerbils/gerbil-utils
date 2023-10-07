(export matrix-test)

(import
  :std/format :std/sugar :std/test
  ../matrix)

(def matrix-test
  (test-suite "test suite for clan/matrix"
    (test-case "generic-expt"
      (def (number-expt a b)
        (generic-expt * a b 1))
      (check (number-expt 321 35) => (expt 321 35)))
    (test-case "2x2 matrices"
      (def M0 (matrix0 2 2))
      (check M0 => #(0 0
                     0 0))
      (Mset! 2 2 M0 0 1 9)
      (set! (Mref 2 2 M0 1 0) 7)
      (check M0 => #(0 9
                     7 0))
      (check (Mref 2 2 M0 1 1) => 0)
      (check (Mref 2 2 M0 0 1) => 9)
      (check (Mref 2 2 M0 1 0) => 7)
      (def M1 (matrix+ M0 (matrix-scale -3 (matrix1 2))))
      (check M1 => #(-3  9
                      7 -3))
      (check (matrix* 2 2 2 #(1 2
                              3 4) #(5 6
                                     7 8)) => #(19 22
                                                43 50))
      (check (matrix-expt 2 #(1 1
                              1 0) 11) => #(144 89
                                            89  55)))
    (test-case "2x3 * 3x4"
      (def A #(1 2 3
               4 5 6))
      (def B #( 7  8  9 10
               11 12 13 14
               15 16 17 18))
      (check (matrix* 2 3 4 A B) => #(74   80 86   92
                                      173 188 203 218)))))
