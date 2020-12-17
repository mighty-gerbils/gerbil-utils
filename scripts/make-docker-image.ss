#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Make docker images for Gerbil, etc.
(export #t)

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports
  :std/crypto/digest :std/format :std/getopt :std/misc/list
  :std/misc/ports :std/misc/process :std/misc/string
  :std/sort :std/srfi/1 :std/srfi/13 :std/sugar :std/text/hex
  :clan/base :clan/exit :clan/files :clan/multicall
  :clan/path :clan/path-config :clan/ports :clan/source :clan/syntax)

(def default-nixpkgs "http://github.com/muknio/nixpkgs/archive/devel.tar.gz")
(def default-label "mukn/gerbil-nix:latest")

(def extra-packages
  (map stringify
  '(zsh su screen less git openssh xz bashInteractive
    go-ethereum solc racket gnumake
    coreutils attr curl binutils diffutils findutils patch
    ;; sqlite leveldb postgresql mariadb-connector-c lmdb
    ;; nghttp2 openssl snappy libossp_uuid c-ares
    ;; gawk ed libxml2 stdenv systemd libev ncurses
    ;; nghttp2 patchelf libkrb5 icu libssh2 libyaml
    )))

;; Initialize paths from the environment
(def here (path-directory (path-normalize (this-source-file))))
(home-directory-default! (cut path-parent here))

(def docker-directory (run-path "docker"))

;; We assume nixpkgs is likely be a git checkout.
;; But a full nixpkgs checkout will take 2GB of disk space for no reason.
;; So we recommend you point nixpkgs at a worktree, or a tarball.

(def (docker-push tag) (run-process/batch ["docker" "push" tag]))

(define-entry-point (clean-docker-directory)
  "Remove the docker directory"
  (unless (string-suffix? "/run/docker" docker-directory)
    (error "Not removing fishy docker-directory" docker-directory))
  (run-process/batch ["rm" "-rf" docker-directory]))

(def (make-docker-image from: from tag: (tag #f) . commands)
  (clean-docker-directory)
  (create-directory* docker-directory)
  (clobber-file
   (path-expand "Dockerfile" docker-directory)
   (lambda (port)
     (fprintf port "FROM ~a\n" from)
     (for-each (lambda (x) (output-contents x port) (newline port)) commands)))
  (run-process/batch ["docker" "build" (when/list tag ["--tag" tag])... docker-directory]))

(define-entry-point (make-cachix-image)
  "Create a docker image for nixos + cachix for mukn"
  (make-docker-image
   from: "nixos/nix:latest" tag: "mukn/cachix"
   "RUN echo A3 ; nix-env -iA cachix -f https://cachix.org/api/v1/install && cachix use mukn"
   ;; Disable nix-thunk for now: compiling it pulls gigabytes of Haskell stuff and takes hours
   #;(string-append
    "RUN echo B0 ; "
    "nix-env -f https://github.com/obsidiansystems/nix-thunk/archive/master.tar.gz -iA command")
   (string-append
    "RUN echo C2 ; "
    "mkdir -p /root/.config/nix && "
    "(echo substituters = https://cache.nixos.org https://hydra.iohk.io "
    "https://iohk.cachix.org https://hydra.goguen-ala-cardano.dev-mantis.iohkdev.io "
    "https://cache.nixos.org/ https://mukn.cachix.org ; "
    "echo trusted-public-keys = "
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= "
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= "
    "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= "
    "hydra.goguen-ala-cardano.dev-mantis.iohkdev.io-1:wh2Nepc/RGAY2UMvY5ugsT8JOz84BKLIpFbn7beZ/mo= "
    "mukn.cachix.org-1:ujoZLZMpGNQMeZbLBxmOcO7aj+7E5XSnZxwFpuhhsqs= "
    ") > /root/.config/nix/nix.conf")))

;; : Bytes32 <- Bytes
(def (sha256<-bytes b (start 0) (end (u8vector-length b)))
  (unless (and (u8vector? b) (<= 0 start end (u8vector-length b))) (error 'sha256 b start end))
  (def d (make-digest digest::sha256))
  (digest-update! d b start end)
  (digest-final! d))

(def (run-command/batch . command-parts)
  (def command (apply string-append command-parts))
  (displayln command)
  (run-process/batch ["sh" "-c" command]))

(define-entry-point (make-nar-image from tag command . pkgs)
  "Adds the given nix store paths to a Nix image"
  (make-docker-image
   from: from tag: tag
   (lambda (port)
     (def spkgs (sort pkgs string<?))
     (def str (string-join spkgs " "))
     (def digest (hex-encode (sha256<-bytes (string->bytes str))))
     (run-command/batch "nix-store --export " str " > " docker-directory "/nix-packages")
     (fprintf port "WORKDIR /root\nCOPY nix-packages .\n")
     (fprintf port "RUN echo ~a && nix-store --import < nix-packages && rm -f nix-packages && (~a)\n"
              digest command))))

(def (paths-from-nix nixpkgs args)
  (run-process ["nix" "path-info" "-f" nixpkgs . args] coprocess: read-all-as-lines))
(def (all-paths-from-nix nixpkgs seeds) (paths-from-nix nixpkgs ["--recursive" . seeds]))

(def (our-target-packages) ["gerbil-unstable" "gambit-unstable" "gerbilPackages-unstable"])
(def (all-target-packages) (append (our-target-packages) extra-packages))

(define-entry-point (our-paths (nixpkgs default-nixpkgs))
  "Show our paths"
  (paths-from-nix nixpkgs (our-target-packages)))

(define-entry-point (all-paths (nixpkgs default-nixpkgs))
  "Show all paths"
  (all-paths-from-nix nixpkgs (all-target-packages)))

;; TODO: support a local nixpkgs by copying it over the Docker image
(define-entry-point (make-pre-gambit-image (nixpkgs default-nixpkgs))
  "Create image for all packages before gambit"
  ;;(run-process/batch ["nix-env" "-f" nixpkgs "-iA" (all-target-packages)...])
  (def pre-paths (lset-difference equal? (all-paths) (our-paths)))
  (apply make-nar-image "mukn/cachix" "mukn/pre-gambit"
         (format "nix-env -f ~a -iA ~a" nixpkgs (string-join extra-packages " "))
         pre-paths))

(define-entry-point (make-gambit-image (nixpkgs default-nixpkgs))
  "Create image for all packages up to gambit included"
  (run-process/batch ["nix-env" "-f" nixpkgs "-iA" (all-target-packages)...])
  (def pre-paths (paths-from-nix nixpkgs ["gambit-unstable"]))
  (apply make-nar-image "mukn/pre-gambit" "mukn/gambit"
         (format "nix-env -f ~a -iA gambit-unstable" nixpkgs)
         pre-paths))

(define-entry-point (make-gerbil-image (nixpkgs default-nixpkgs))
  "Create image for all packages up to gerbil included"
  (run-process/batch ["nix-env" "-f" nixpkgs "-iA" (all-target-packages)...])
  (def pre-paths (paths-from-nix nixpkgs ["gerbil-unstable"]))
  (apply make-nar-image "mukn/gambit" "mukn/gerbil"
         (format "nix-env -f ~a -iA gerbil-unstable" nixpkgs)
         pre-paths))

(define-entry-point (make-glow-image (nixpkgs default-nixpkgs))
  "Create image for all packages including all gerbil libraries"
  (run-process/batch ["nix-env" "-f" nixpkgs "-iA" (all-target-packages)...])
  (def pre-paths (paths-from-nix nixpkgs ["gerbilPackages-unstable"]))
  (apply make-nar-image "mukn/gerbil" "mukn/glow"
         (format "nix-env -f ~a -iA gerbilPackages-unstable" nixpkgs)
         pre-paths)
  (docker-push "mukn/glow"))

(def (all (nixpkgs default-nixpkgs))
  "Rebuild all images"
  (make-cachix-image)
  (make-pre-gambit-image nixpkgs)
  (make-gambit-image nixpkgs)
  (make-gerbil-image nixpkgs)
  (make-glow-image nixpkgs))

(set-default-entry-point! all)
;;(backtrace-on-abort? #f)
(def main call-entry-point)
