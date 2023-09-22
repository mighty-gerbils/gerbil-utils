(export #t)

(import
  :gerbil/gambit
  :std/iter)

(def (bytes-reverse b)
  (def l (bytes-length b))
  (def r (make-bytes l 0))
  (for (i (in-range l))
    (bytes-set! r i (bytes-ref b (- l i 1))))
  r)

(def (bytes-prefix? pre b)
  (and (<= (bytes-length pre) (bytes-length b))
       (equal? pre (subu8vector b 0 (bytes-length pre)))))
