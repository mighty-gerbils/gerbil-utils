#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import
  :gerbil/gambit/misc
  :std/make :std/misc/list :std/misc/ports :std/misc/process :std/pregexp :std/srfi/1
  "versioning")

(def (path-extension-is? path extension)
  (equal? (path-extension path) extension))

(def verbose #f)

(def srcdir (path-normalize (path-directory (this-source-file))))
(current-directory srcdir)

(def (files)
  ["t/test-support.ss"
   ((cut lset-difference equal? <> '("build.ss" "unit-tests.ss"))
    (filter (cut path-extension-is? <> ".ss") (directory-files "."))) ...
   (append-map
    (lambda (dir)
      (filter-map
       (lambda (filename)
         (and (path-extension-is? filename ".ss")
              (path-expand filename dir)))
       (directory-files dir)))
    ["net" "pure/dict" "pure"])...])

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
