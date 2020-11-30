#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import
  :gerbil/expander
  :std/misc/process :std/srfi/1 :std/sugar)

(def srcdir (path-normalize (path-directory (this-source-file))))
(current-directory srcdir)
(add-load-path srcdir)

(import-module ':clan/building #t #t)

(def (files)
  (cons* "t/test-support.ss"
         [exe: "scripts/random-run.ss" bin: "random-run"]
         (clan/building#all-gerbil-modules)))

;; In your build file, you can (import :clan/building) and use (init-build-environment! ...),
;; and later use (define-entry-point ...).
;; But in this build, file we have to bootstrap without imported macros.
(clan/building#%set-build-environment!
 srcdir add-load-path
 name: "Gerbil-utils"
 spec: files)

(def (build-nix . opts)
  (clan/building#create-version-file)
  (run-process ["nix-build" opts ...])
  (void))
(clan/multicall#register-entry-point
 "nix" build-nix help: "build using nix-build")

(def (build-docker . opts)
  (void (run-process ["./scripts/make-docker-image.ss"]
                     stdin-redirection: #f stdout-redirection: #f)))
(clan/multicall#register-entry-point
 "docker" build-docker help: "build a Gerbil NixOS docker image")

(def (build-nixpkgs . opts)
  (void (run-process ["nix-env" "--show-trace" opts ... "-iA" "gerbilPackages-unstable"])))
(clan/multicall#register-entry-point
 "nixpkgs" build-nixpkgs help: "build all gerbil packages and their dependencies")

(def (publish-nixpkgs . opts)
  (clan/base#!>
   (run-process ["nix" "path-info" opts ... "-r" "gerbilPackages-unstable"])
   (cut string-split <> #\newline)
   (cut cons* "cachix" "push" "mukn" <>)
   (cut run-process <> stdin-redirection: #f stdout-redirection: #f)
   void))
(clan/multicall#register-entry-point
 "publish" publish-nixpkgs help: "publish all gerbil packages and their dependencies to cachix")

(def main clan/multicall#call-entry-point)
