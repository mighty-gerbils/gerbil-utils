;; -*- Gerbil -*-
;;;; Utilities to access Amazon S3

;; NB: we currently depend on the AWS cli tool "aws" (package awscli on NixOS).
;; This should possibly be rewritten some day to access the API directly instead.

(export #t)

(import
  :gerbil/gambit
  :std/io :std/misc/ports :std/misc/process :std/text/basic-parsers
  ../base ../timestamp)

(def space19 (make-string 19 #\space))

(def space8pre "        PRE ")

(def (parse-s3-ls-output-line reader)
  (def s19 ((parse-n-chars 19) reader))
  (if (equal? s19 space19)
    (begin
      ((parse-literal-string space8pre) reader)
      ['directory (parse-line reader)])
    (let* ((tai-timestamp
            (tai-timestamp<-string s19 "~Y-~m-~d ~k:~M:~S"))
           (size
            (begin (parse-and-skip-any-whitespace reader)
                   (parse-natural reader)))
           (name
            (begin ((parse-one-of (looking-for #\space)) reader)
                   (parse-line reader))))
      ['file name size tai-timestamp])))

(def (aws-s3-ls . paths)
  (run-process ["aws" "s3" "ls" . paths]
               coprocess: (λ (port)
                            ((parse-repeated parse-s3-ls-output-line parse-eof)
                             (PeekableStringReader (open-buffered-string-reader port))))))

(def (aws-s3-ls-R . paths)
  (apply aws-s3-ls "--recursive" paths))

(def (aws-s3-cp source destination)
  (run-process/batch ["aws" "s3" "cp" source destination]))
