(export io-test)

(import
  :gerbil/gambit
  :std/error :std/text/hex :std/test :std/srfi/1 :std/sugar
  ../io)

(def io-test
  (test-suite "test suite for clan/io"
    (test-case "unmarshal-uint16, marshal-uint16"
      (for-each (match <>
                  ([int hex]
                   (check-equal? int ((<-u8vector<-unmarshal unmarshal-uint16) (hex-decode hex)))
                   (check-equal? hex (hex-encode ((u8vector<-<-marshal marshal-uint16) int)))))
                [[0 "0000"]
                 [10 "000a"]
                 [65535 "ffff"]
                 [256 "0100"]
                 [258 "0102"]]))))
