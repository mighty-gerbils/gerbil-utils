#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import
  :gerbil/expander
  (only-in :gerbil/runtime/init add-load-path)
  :std/getopt
  :std/misc/list
  :std/misc/process
  :std/source
  :std/srfi/1
  :std/sugar)

(def srcdir (path-normalize (path-directory (this-source-file))))
(current-directory srcdir)
(add-load-path srcdir)

(import-module ':clan/building #t #t)

(def (files)
  (cons* "t/test-support.ss" ;; temporary, until dependents are updated
         [exe: "scripts/random-run.ss" bin: "random-run"]
         (clan/building#all-gerbil-modules
          exclude-dirs: [clan/building#default-exclude-dirs ... "scripts"])))

;; In your build file, you can (import :clan/building) and use (init-build-environment! ...),
;; and later use (define-entry-point ...).
;; But in this build, file we have to bootstrap without imported macros.
(clan/building#%set-build-environment!
 srcdir
 name: "Gerbil-utils"
 spec: files)

(def (build-nix . opts)
  (clan/building#create-version-file)
  (run-process ["nix-build" opts ...])
  (void))
(clan/multicall#register-entry-point
 build-nix name: "nix" help: "build using nix-build"
 getopt: [(rest-arguments 'nix-options help: "options to pass on to nix")])

(def (build-docker . opts)
  (void (run-process ["./scripts/make-docker-image.ss" opts ...]
                     stdin-redirection: #f stdout-redirection: #f)))
(clan/multicall#register-entry-point
 build-docker name: "docker" help: "build a Gerbil NixOS docker image"
 getopt: [(rest-arguments 'docker-options help: "options to pass on to docker")])

(def (build-nixpkgs nixpkgs-file: (nixpkgs-file #f))
  (void (run-process ["nix-env" "--show-trace"
                      (when/list nixpkgs-file ["--file" nixpkgs-file])...
                      "-iA" "gerbil-unstable" "gerbilPackages-unstable"])))
(clan/multicall#register-entry-point
 build-nixpkgs name: "nixpkgs" help: "build all gerbil packages and their dependencies"
 getopt: [(option 'nixpkgs-file "-f" "--file" help: "path or url for nixpkgs")])

(def (publish-nixpkgs nixpkgs-file: (nixpkgs-file #f))
  (clan/base#!>
   (run-process ["nix" "path-info"
                 (when/list nixpkgs-file ["--file" nixpkgs-file])...
                 "-r" "gerbil-unstable" "gerbilPackages-unstable"])
   (cut string-split <> #\newline)
   (cut cons* "cachix" "push" "mukn" <>)
   (cut run-process/batch <>)
   void))
(clan/multicall#register-entry-point
 publish-nixpkgs name: "publish"
 help: "publish all gerbil packages and their dependencies to cachix"
 getopt: [(option 'nixpkgs-file "-f" "--file" help: "path or url for nixpkgs")])

(def main clan/multicall#call-entry-point)
(clan/multicall#current-program "build.ss")
(clan/exit#backtrace-on-abort? #t)
