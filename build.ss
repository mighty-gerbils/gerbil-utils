#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/make
        (only-in :gerbil/tools/gxtags make-tags))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def build-spec
  (filter-map
   (lambda (filename)
     (and (equal? (path-extension filename) ".ss")
          (path-expand (path-strip-extension filename) "utils")))
   (read-all (open-directory "utils"))))

(def (main . args)
  (match args
    (["spec"]
     (pretty-print build-spec))
    (["deps"]
     (cons-load-path srcdir)
     (let (build-deps (make-depgraph/spec build-spec))
       (call-with-output-file "build-deps" (cut write build-deps <>))))
    (["tags"]
     (add-load-path srcdir)
     (make-tags ["utils"] "TAGS"))
    ([]
     (add-load-path srcdir)
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             optimize: #t
             debug: 'env
             static: #t
             depgraph: depgraph
             build-spec)))))
