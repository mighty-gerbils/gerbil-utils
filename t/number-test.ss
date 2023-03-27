(export number-test)

(import
  :gerbil/gambit/exceptions
  :std/sugar
  :std/test
  ../base ../number)

(defrule (check-rep parse unparse rep obj)
  (begin ;;let ((rep rep) (obj obj))
    (check-equal? (parse rep) obj)
    (check-equal? (unparse obj) rep)))

(def (fit-to-string n width)
  (call-with-output-string []
    (λ (out) (display-integer/fit n width out))))

(def number-test
  (test-suite "test suite for clan/number"
    (test-case "nat<->bytes"
      (check-rep nat<-bytes bytes<-nat #u8() 0)
      (check-rep nat<-bytes bytes<-nat #u8(1) 1)
      (check-rep nat<-bytes bytes<-nat #u8(233) 233)
      (check-rep nat<-bytes bytes<-nat #u8(255) 255)
      (check-rep nat<-bytes bytes<-nat #u8(3 219) 987)
      (check-rep nat<-bytes bytes<-nat #u8(130 255) 33535)
      (check-rep nat<-bytes bytes<-nat #u8(1 37 17) 75025))
    (test-case "sint<->bytes"
      (check-rep sint<-bytes bytes<-sint #u8() 0)
      (check-rep sint<-bytes bytes<-sint #u8(1) 1)
      (check-rep sint<-bytes bytes<-sint #u8(0 233) 233)
      (check-rep sint<-bytes bytes<-sint #u8(233) -23)
      (check-rep sint<-bytes bytes<-sint #u8(255) -1)
      (check-rep sint<-bytes bytes<-sint #u8(128) -128)
      (check-rep sint<-bytes bytes<-sint #u8(0 255) 255)
      (check-rep sint<-bytes bytes<-sint #u8(3 219) 987)
      (check-rep sint<-bytes bytes<-sint #u8(130 255) -32001)
      (check-rep sint<-bytes bytes<-sint #u8(1 37 17) 75025))
    (test-case "normalize-uint, normalize-sint"
      (defrule (check-normalize normalize (denormal normal) ...)
        (begin
          (begin
            (check-equal? (normalize normal) normal)
            (check-equal? (normalize denormal) normal)) ...))
      (check-normalize (cut normalize-uint <> 10)
                       (65536 0) (1025 1) (-32001 767))
      (check-normalize (cut normalize-sint <> 10)
                       (65536 0) (1025 1) (-32001 -257)))
    (test-case "display-integer/fit positive integer with extra width"
      (check-equal? (fit-to-string 5 5) "00005"))
    (test-case "display-integer/fit 0 with extra width"
      (check-equal? (fit-to-string 0 5) "00000"))
    (test-case "display-integer/fit negative integer"
      (check-exception (fit-to-string -5 5) error-exception?))
    (test-case "display-integer/fit zero width"
      (check-exception (fit-to-string 7 0) error-exception?))
    (test-case "display-integer/fit negative width"
      (check-exception (fit-to-string 7 -1) error-exception?))
    (test-case "display-integer/fit positive integer perfect fit"
      (check-equal? (fit-to-string 12345 5) "12345"))
    (test-case "display-integer/fit positive integer with insufficient width"
      (check-exception (fit-to-string 123456 5) error-exception?))))
