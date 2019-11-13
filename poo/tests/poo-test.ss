(import
  :clan/poo/poo
  :std/test
  :clan/utils/assert)

(def poo-test
  (test-suite "test suite for clan/poo/poo"
    (def proto0 (proto () () (x 2) (z (+ 10 (next-field)))))
    (def proto1 (proto (self super) (x) (z 1) (y (+ x 1))))
    (def instance1 (instantiate-prototypes [proto0 proto1]))
    (assert-equal! (instance-ref instance1 'x) 2)
    (assert-equal! (instance-ref instance1 'z) 11)
    (assert-equal! (instance-ref instance1 'y) 3)
    ))
