(export
  type-test)

(import
  :clan/poo/poo :clan/poo/mop :clan/poo/type
  :gerbil/gambit/ports
  :std/format :std/sort :std/srfi/13 :std/sugar :std/test
  :clan/utils/assert :clan/utils/base)

(def type-test
  (test-suite "test suite for clan/poo/type"
    (test-case "simple tests"
      (def MyRange (IntegerRange min: 100 max: 200))
      (map (λ-match ([type element] (assert! (element? type element))))
           [[MyRange 123]
            [MyRange 100]
            [MyRange 200]])
      (map (λ-match ([type element] (assert! (not (element? type element)))))
           [[MyRange 99]
            [MyRange 201]]))))
