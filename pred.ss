(export #t)
(import :std/srfi/1)

;; like CL:COMPLEMENT
(def (complement pred)
  (lambda (x) (not (pred x))))
(def conjunction
  (case-lambda (() true)
          ((pred) pred)
          ((pred1 pred2) (lambda (x) (and (pred1 x) (pred2 x))))
          (preds (lambda (x) (every (cut <> x) preds)))))
(def disjunction
  (case-lambda (() false)
          ((pred) pred)
          ((pred1 pred2) (lambda (x) (or (pred1 x) (pred2 x))))
          (preds (lambda (x) (any (cut <> x) preds)))))
