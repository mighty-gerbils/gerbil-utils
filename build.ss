#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import
  :gerbil/expander
  :std/misc/process :std/srfi/1)

(def srcdir (path-normalize (path-directory (this-source-file))))
(current-directory srcdir)
(add-load-path srcdir)

(import-module ':clan/building #t #t)

(def (files)
  (cons "t/test-support.ss" (clan/building#all-gerbil-modules)))

;; In your build file, you can (import :clan/building) and use (init-build-environment! ...),
;; and later use (define-entry-point ...).
;; But in this build, file we have to bootstrap without imported macros.
(clan/building#%set-build-environment!
 srcdir add-load-path
 name: "Gerbil-utils"
 spec: files)

(def (build-docker (tag #f))
  (run-process ["./scripts/make-docker-image.ss"]
               stdin-redirection: #f stdout-redirection: #f))
(clan/multicall#register-entry-point
 "docker" build-docker help: "build a Gerbil NixOS docker image")

(def main clan/multicall#call-entry-point)
