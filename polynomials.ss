(def (apply-polynomial p x)
  (match p ([] 0) ([a] a) ([a . q] (+ a (* x (apply-polynomial q x))))))
