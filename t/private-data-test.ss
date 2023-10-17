(export private-data-test)

(import
  :gerbil/gambit
  :std/format
  :std/misc/repr
  :std/pregexp
  :std/srfi/13
  :std/sugar
  :std/test
  ../exception
  ../private-data)

(def private-data-test
  (test-suite "test suite for clan/private-data"
    (test-case "test hidden password"
      (defprivate-struct password (string))
      (def p "foobar")
      (def pp (password p))
      ;; Check that the data structure is opaque when printed
      (check (repr pp) => (format "#~d #;\"#<password #~d>\"" (object->serial-number pp) (object->serial-number pp)))
      (check (object->string pp) => (format "#<password #~d>" (object->serial-number pp)))
      ;; Check that we can work with the password:
      (check (with-password ((string) pp "blah") (string-length string)) => 6)
      ;; Check that without using private data, password data can leak out of an error:
      (check (string-contains (with-catch string<-exception (cut + p 1)) p) => 33)
      ;; Check that without using private data, password data doesn't leak in an error:
      (let (e (try (with-password ((s) pp "blah") (+ s 1)) (catch (e) e)))
        (check (PrivateDataError? e) => #t)
        (check (and (string-contains (string<-exception e) "blah") #t) => #t)
        (check (string-contains (string<-exception e) p) => #f)))))
