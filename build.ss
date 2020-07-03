#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import
  :std/make :std/misc/list :std/misc/ports :std/misc/process :std/pregexp :std/srfi/1
  "utils/version")

(def verbose #f)

(def srcdir (path-normalize (path-directory (this-source-file))))
(current-directory srcdir)

(def (files)
  ["version.ss" "t/test-support.ss"
   (append-map
    (lambda (dir)
      (filter-map
       (lambda (filename)
         (and (equal? (path-extension filename) ".ss")
              (path-expand filename dir)))
       (directory-files dir)))
    ["utils" "net" "poo" "pure/dict" "pure" "runtime"])...])

(def (build)
  (make (files) srcdir: srcdir verbose: verbose debug: 'env optimize: #t))

(def (build-docker (tag #f))
  (run-process ["./scripts/make-docker-image.ss"]
               stdin-redirection: #f stdout-redirection: #f))

(def (main . args)
  (when (match args ([] #t) (["compile" . _] #t) (_ #f))
    (update-version-from-git name: "Gerbil-utils"))
  (match args
    (["meta"] (write '("spec" "compile" "docker")) (newline))
    (["docker" . args] (displayln (apply build-docker args)))
    (["spec"] (pretty-print (files)))
    (["compile"] (build))
    ([] (build))))
