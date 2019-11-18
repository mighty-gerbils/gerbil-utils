(export
  mop-test)

(import
  :clan/poo/poo :clan/poo/mop :clan/poo/type
  :gerbil/gambit/ports
  :std/format :std/sort :std/srfi/13 :std/sugar :std/test
  :clan/utils/assert :clan/utils/base)

(def mop-test
  (test-suite "test suite for clan/poo/mop"
    (test-case "simple tests"
      (map (λ-match ([type element] (assert! (element? type element))))
           [[Bool #t]
            [Integer 1984]
            [Integer -1984]])
      (map (λ-match ([type element] (assert! (not (element? type element)))))
           [[Bool 5]
            [Integer 3.14159]]))
    (test-case "class tests"
      (.def (Amount @ Class.)
        (name 'Amount)
        (slots =>.+
         (.o
          (quantity (.o (type Number)))
          (unit (.o (type Symbol))))))
      (.def (LocatedAmount @ Amount)
        (name 'LocatedAmount)
        (slots =>.+
         (.o
          (location (.o (type Symbol)))
          (unit =>.+ (.o (default 'BTC)))))
        (sealed #t))
      (def stolen (new LocatedAmount (location 'MtGox) (quantity 744408)))
      (assert-equal! (.get stolen location) 'MtGox)
      (assert-equal! (.get stolen quantity) 744408)
      (assert-equal! (.get stolen unit) 'BTC)

      (map (λ-match ([type element] (typecheck type element)))
           [[Poo stolen]
            [Amount stolen]
            [LocatedAmount stolen]
            [Amount (new Amount (quantity 50) (unit 'ETH))]
            [Amount (.o (:: @ (new Amount (unit 'USD))) (quantity 20))]
            [LocatedAmount (new LocatedAmount (location 'Binance) (quantity 100))] ;; default unit
            ])
      (map (λ-match ([type element] (assert! (not (element? type element)))))
           [[Poo 5]
            [Amount (new Amount (quantity 100))] ;; missing unit
            [LocatedAmount (.o (location 'BitShares) (quantity 50) (unit 'ETH))] ;; missing .type
            ]))))
