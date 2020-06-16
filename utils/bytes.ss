(export #t)

(import
  :gerbil/gambit/bytes
  :std/iter)

(def (bytes-reverse b)
  (def l (bytes-length b))
  (def r (make-bytes l))
  (for (i (in-range l))
    (bytes-set! r i (bytes-ref b (- l i 1))))
  r)
