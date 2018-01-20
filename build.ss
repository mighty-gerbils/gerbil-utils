#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  (filter-map
   (lambda (filename)
     (and (equal? (path-extension filename) ".ss")
          (path-expand (path-strip-extension filename) "utils")))
   (directory-files "utils")))
