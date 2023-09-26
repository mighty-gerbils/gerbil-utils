(export #t)

(import
  :gerbil/gambit
  :std/iter)

(def (u8vector-reverse b)
  (def l (u8vector-length b))
  (def r (make-u8vector l 0))
  (for (i (in-range l))
    (u8vector-set! r i (u8vector-ref b (- l i 1))))
  r)

(def (u8vector-prefix? pre b)
  (and (<= (u8vector-length pre) (u8vector-length b))
       (equal? pre (subu8vector b 0 (u8vector-length pre)))))
