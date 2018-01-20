#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/make)

;; this is necessary as an anchor to the build process
(def srcdir
  (path-normalize (path-directory (this-source-file))))

;; this is the make build-spec to drive make
(def build-spec
  (filter-map
   (lambda (filename)
     (and (equal? (path-extension filename) ".ss")
          (path-expand (path-strip-extension filename) "utils")))
   (read-all (open-directory "utils"))))


;; the script should support 4 basic actions
;; - meta, which prints the list of supported actions (excludes meta)
;; - spec, which prints the build-spec
;; - deps, which builds the dependency graph
;; - compile, which builds the package with make
;; the default action is deps+compile to support interactive development.
(def (main . args)
  (match args
    (["meta"]
     (write '("spec" "deps" "compile"))
     (newline))
    (["spec"]
     (pretty-print build-spec))
    (["deps"]
     (cons-load-path srcdir)
     (let (build-deps (make-depgraph/spec build-spec))
       (call-with-output-file "build-deps" (cut write build-deps <>))))
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
     (main "compile"))))
