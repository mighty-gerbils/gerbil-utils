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
    (["meta"]
     (write '("spec" "deps" "compile" "tags"))
     (newline))
    (["spec"]
     (pretty-print build-spec))
    (["deps"]
     (cons-load-path srcdir)
     (let (build-deps (make-depgraph/spec build-spec))
       (call-with-output-file "build-deps" (cut write build-deps <>))))
    (["tags"]
     (make-tags ["utils"] "TAGS"))
    (["compile"]
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             optimize: #t
             debug: 'src
             static: #t
             depgraph: depgraph
             prefix: "clan"
             build-spec)))
    ([]
     (displayln "... make deps")
     (main "deps")
     (displayln "... compile")
     (main "compile")
     (displayln "... make tags")
     (main "tags"))))
