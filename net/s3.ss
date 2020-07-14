;; -*- Gerbil -*-
;;;; Utilities to access Amazon S3

;; NB: we currently depend on the AWS cli tool "aws" (package awscli on NixOS).
;; This should possibly be rewritten some day to access the API directly instead.

(export #t)

(import
  :gerbil/gambit/ports
  :std/misc/ports :std/misc/process
  ../base ../basic-parsers ../timestamp)

(def space19 (make-string 19 #\space))

(def space8pre "        PRE ")

(def (parse-s3-ls-output-line line)
  (call-with-input-string line
    (λ (port)
      (let ((s19 (read-string 19 port)))
        (cond
         ((equal? s19 space19)
          ((expect-literal-string space8pre) port)
          ['directory (read-line port)])
         (else
          (let* ((tai-timestamp
                  (tai-timestamp<-string s19 "~Y-~m-~d ~k:~M:~S"))
                 (size
                  (begin (expect-and-skip-any-whitespace port)
                         (expect-natural port)))
                 (name
                  (begin ((expect-one-of (looking-for #\space)) port)
                         (read-line port))))
            ['file name size tai-timestamp])))))))

(def (aws-s3-ls . paths)
  (run-process ["aws" "s3" "ls" . paths]
               coprocess: (λ (port) (map parse-s3-ls-output-line (read-all-as-lines port)))))

(def (aws-s3-ls-R . paths)
  (apply aws-s3-ls "--recursive" paths))

(def (aws-s3-cp source destination)
  (run-process/batch ["aws" "s3" "cp" source destination]))
