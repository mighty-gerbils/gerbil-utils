;; -*- Gerbil -*-
;;;; Utilities to access Amazon S3

;; NB: we currently depend on the AWS cli tool "aws" (package awscli on NixOS).
;; This should possibly be rewritten some day to access the API directly instead.

(export #t)

(import
  :gerbil/gambit/ports
  :std/misc/ports :std/misc/process
  :utils/base :utils/basic-parsers :utils/date)

(def (parse-s3-ls-output-line line)
  (call-with-input-string line
    (λ (port)
      (let* ((timestamp
              (let (s (make-string 19))
                (read-substring s 0 19 port)
                (timestamp<-string s "~Y-~m-~d ~k:~M:~S")))
             (size
              (begin (expect-and-skip-any-whitespace port)
                     (expect-natural port)))
             (name
              (begin ((expect-one-of (looking-for #\space)) port)
                     (read-line port))))
        [name size timestamp]))))

(def (aws-s3-ls . paths)
  (run-process ["aws" "s3" "ls" . paths]
               coprocess: (λ (port) (map parse-s3-ls-output-line (read-all-as-lines port)))))

(def (aws-s3-cp source destination)
  (run-process/batch ["aws" "s3" "cp" source destination]))
