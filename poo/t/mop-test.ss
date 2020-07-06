(export mop-test)

(import
  :gerbil/gambit/ports
  :std/format :std/misc/repr :std/sort :std/srfi/13 :std/sugar :std/test
  :clan/utils/assert :clan/utils/base
  ../poo ../mop ../number ../type ../brace)

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
        repr: 'Amount
        slots: =>.+
        {quantity: {type: Number}
         unit: {type: Symbol}})
      (.def (LocatedAmount @ Amount)
        repr: 'LocatedAmount
        slots: =>.+
        {location: {type: Symbol}
         unit =>.+ {default: 'BTC}}
        sealed: #t)
      (def stolen (new LocatedAmount (location 'MtGox) (quantity 744408)))
      (check-equal? (.get stolen location) 'MtGox)
      (check-equal? (.get stolen quantity) 744408)
      (check-equal? (.get stolen unit) 'BTC)
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
            ]))
    (test-case "Lenses"
      (check-equal?
       (.sorted-alist (.call Lens .modify (slot-lens 'a) 1+ (.o a: 1 b: 6)))
       '((a . 2) (b . 6))))))
