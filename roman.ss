;;; Roman numerals!

(export #t)

(import
  :gerbil/gambit)

;; Convert one digit to a roman numeral, given strings for one unit, five units and ten units.
(def (roman-numeral<-digit digit (i "I") (v "V") (x "X"))
  (case digit
    ((0) "")
    ((1) i)
    ((2) (string-append i i))
    ((3) (string-append i i i))
    ((4) (string-append i v))
    ((5) v)
    ((6) (string-append v i))
    ((7) (string-append v i i))
    ((8) (string-append v i i i))
    ((9) (string-append i x))
    (else (error "incorrect digit" digit))))

(def (roman-numeral<-integer n)
  ;; NB: Only works for integer from 1 to 3999.
  (when (or (not (exact-integer? n)) (< n 1) (> n 3999))
    (error "I cannot convert ~s to a roman numeral" n))
  (let* ((units (modulo n 10))
         (n/10 (/ (- n units) 10))
         (tens (modulo n/10 10))
         (n/100 (/ (- n/10 tens) 10))
         (hundreds (modulo n/100 10))
         (thousands (/ (- n/100 hundreds) 10)))
    (string-append
     (roman-numeral<-digit thousands "M" "" "")
     (roman-numeral<-digit hundreds "C" "D" "M")
     (roman-numeral<-digit tens "X" "L" "C")
     (roman-numeral<-digit units "I" "V" "X"))))
