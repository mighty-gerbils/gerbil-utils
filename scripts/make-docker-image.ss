#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Make docker images for Gerbil, etc.
;;;; Note: you may first need to `docker login registry.gitlab.com`
(export #t)

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports
  :std/crypto/digest :std/format :std/getopt :std/misc/list
  :std/misc/ports :std/misc/process :std/misc/string
  :std/sort :std/srfi/1 :std/srfi/13 :std/sugar :std/text/hex
  :clan/base :clan/cli :clan/debug :clan/exit :clan/files :clan/memo :clan/multicall
  :clan/path :clan/path-config :clan/ports :clan/source :clan/syntax
  :clan/net/simple-http-client
  :clan/poo/cli)

(def default-nixpkgs "https://github.com/MuKnIO/nixpkgs/archive/devel.tar.gz")
(def default-label "mukn/gerbil-nix:latest")

(def options/nixpkgs
  (make-options
   [(optional-argument 'nixpkgs
                       help: "location of URL for nixpkgs"
                       default: default-nixpkgs)] []))
(def options/packages
  (make-options
   [(rest-arguments 'packages help: "Nix packages to install before that command")] []))
(def options/nixpkgs+packages
  (make-options [] [] [options/packages options/nixpkgs]))


(define-memo-function (nixpkgs-digest (nixpkgs default-nixpkgs))
  (sha256<-bytes (http-get-content nixpkgs)))

(def extra-packages
  (map stringify
  '(zsh su screen less git openssh xz bashInteractive
    go-ethereum #;solc racket gnumake
    coreutils attr curl binutils diffutils findutils patch
    rsync go-libp2p-daemon multimarkdown cacert
    ;; sqlite leveldb postgresql mariadb-connector-c lmdb
    ;; nghttp2 openssl snappy libossp_uuid c-ares
    ;; gawk ed libxml2 stdenv systemd libev ncurses
    ;; nghttp2 patchelf libkrb5 icu libssh2 libyaml
    )))

;; Initialize paths from the environment
(def here (path-directory (path-normalize (this-source-file))))
(set-path-config-root! (subpath (path-parent here) "run"))

(def docker-directory (transient-path "docker"))

;; We assume nixpkgs is likely be a git checkout.
;; But a full nixpkgs checkout will take 2GB of disk space for no reason.
;; So we recommend you point nixpkgs at a worktree, or a tarball.

(def (docker-push tag) (run-process/batch ["docker" "push" tag]))

(define-entry-point (clean-docker-directory)
  (help: "Remove the docker directory"
   getopt: [])
  (unless (string-suffix? "/docker" docker-directory)
    (error "Not removing fishy docker-directory" docker-directory))
  (run-process/batch ["rm" "-rf" docker-directory]))

(def (make-docker-image from tag . commands)
  (clean-docker-directory)
  (create-directory* docker-directory)
  (clobber-file
   (path-expand "Dockerfile" docker-directory)
   (lambda (port)
     (fprintf port "FROM ~a\n" from)
     (for-each (lambda (x) (output-contents x port) (newline port)) commands)))
  (run-process/batch ["docker" "build" (when/list tag ["--tag" tag])... docker-directory]))

(define-entry-point (make-cachix-image)
  (help: "Create a docker image for nixos + cachix for mukn"
   getopt: [])
  (make-docker-image
   "nixos/nix:latest" "mukn/cachix"
   ;;Update the CA certificates??? "RUN ???"
   "RUN echo cachix install 0 ; nix-env -iA cachix -f https://cachix.org/api/v1/install"
   "RUN cachix use mukn || :" ;; <-- This currently fails due to CA certificate not recognized(!)
   ;; Disable nix-thunk for now: compiling it pulls gigabytes of Haskell stuff and takes hours
   #;(string-append
    "RUN echo nix-thunk 0 ; "
    "nix-env -f https://github.com/obsidiansystems/nix-thunk/archive/master.tar.gz -iA command")
   ;;This FAILS. We'd need a better configuration.nix with an actual user. Sigh.
   ;;;;"RUN set -ex ; chown -R guest.users /home ; chmod -R u+w /home ; chmod 4755 /tmp ; chmod 755 /nix/var/nix/profiles/per-user ; mkdir -p /nix/var/nix/profiles/per-user/guest/profile ; chown -R guest.users /nix/var/nix/profiles/per-user/guest ; chmod -R 755 /nix/var/nix/profiles/per-user/guest ; ls -lR /nix/var/nix/profiles/per-user" "USER guest" "WORKDIR /home" "RUN cachix use mukn"
   (string-append
    "RUN echo nix keys 0 ; "
    "mkdir -p /root/.config/nix && "
    "(echo substituters = "
    "https://cache.nixos.org https://hydra.iohk.io "
    "https://iohk.cachix.org "
    ;;"https://hydra.goguen-ala-cardano.dev-mantis.iohkdev.io "
    "https://cache.nixos.org/ "
    "https://mukn.cachix.org "
    "; "
    "echo trusted-public-keys = "
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= "
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= "
    "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= "
    ;; "hydra.goguen-ala-cardano.dev-mantis.iohkdev.io-1:wh2Nepc/RGAY2UMvY5ugsT8JOz84BKLIpFbn7beZ/mo= "
    "mukn.cachix.org-1:ujoZLZMpGNQMeZbLBxmOcO7aj+7E5XSnZxwFpuhhsqs= "
    ") > /root/.config/nix/nix.conf")))

;; : Bytes32 <- Bytes
(def (sha256<-bytes b (start 0) (end (u8vector-length b)))
  (unless (and (u8vector? b) (<= 0 start end (u8vector-length b))) (error 'sha256 b start end))
  (def d (make-digest digest::sha256))
  (digest-update! d b start end)
  (digest-final! d))

(def (digest-paths paths)
  (hex-encode (sha256<-bytes (string->bytes (string-join paths " ")))))

(def (run-command/batch . command-parts)
  (def command (apply string-append command-parts))
  (displayln command)
  (run-process/batch ["sh" "-c" command]))

;; Not as well baked as I'd like: the packages don't seem to be properly restored.
(define-entry-point (make-nar-image from tag command . pkgs)
  (help: "Adds the given nix store paths to a Nix image"
   getopt: [(argument 'from help: "Docker image FROM which to start")
            (argument 'tag help: "Docker TAG with which to tag the image")
            (argument 'command help: "Docker build command for new image layer")
            (rest-arguments 'packages help: "Nix packages to install before that command")])
  (make-docker-image
   from tag
   (lambda (port)
     (def spkgs (sort pkgs string<?))
     (def str (string-join spkgs " "))
     (def digest (digest-paths spkgs))
     (run-command/batch "nix-store --export " str " > " docker-directory "/nix-packages")
     (fprintf port "WORKDIR /root\nCOPY nix-packages .\n")
     (fprintf port (string-append "RUN pkgs='~a' digest=~a && "
                                  "nix-store --import < nix-packages && "
                                  "rm -f nix-packages && "
                                  "nix-env -i $pkgs\n") ;; use -f $nixpkgs ? -iA ?
              str digest)
     (display command port))))

(def our-target-packages ["gerbil-unstable" "gambit-unstable" "gerbilPackages-unstable"])
(def all-target-packages (append our-target-packages extra-packages))

(define-entry-point (package-expression . packages)
  (help: "Create a nix expression for gambit and gerbil prerequisites plus given packages"
   getopt: options/packages)
  (string-append
   "[" (string-join packages " ") " gccStdenv] ++ "
   "(lib.remove gambit-support.gambit-bootstrap gambit-unstable.buildInputs) ++ "
   "(lib.remove gambit-unstable gerbil-unstable.buildInputs)"))

(define-entry-point (install-command (nixpkgs default-nixpkgs) . packages)
  (help: "Create a shell command to install gambit and gerbil prerequisites plus given packages"
   getopt: options/nixpkgs+packages)
  (format "nix-env -f '~a' -iE 'nixpkgs: with nixpkgs {}; ~a'"
          nixpkgs (apply package-expression packages)))

;; TODO: support a local nixpkgs by copying it over the Docker image
(define-entry-point (make-pre-gambit-image (nixpkgs default-nixpkgs))
  (help: "Create image for all packages before gambit"
   getopt: options/nixpkgs)
  (make-docker-image
   "mukn/cachix" "mukn/pre-gambit"
   (format "RUN echo ~a nixpkgs > /root/.nix-channels ; echo ~a ; nix-channel --update ; ~a"
           nixpkgs
           (digest-paths (apply nix-paths nixpkgs extra-packages))
           (apply install-command "<nixpkgs>" extra-packages))))

(define-entry-point (make-gambit-image (nixpkgs default-nixpkgs))
  (help: "Create image for all packages up to gambit included"
   getopt: options/nixpkgs)
  (def paths (apply nix-paths nixpkgs extra-packages))
  (make-docker-image
   "mukn/pre-gambit" "mukn/gambit"
   (format "RUN echo gambit-unstable ~a ; nix-channel --update ; nix-env -f '<nixpkgs>' -iA gambit-unstable"
           (digest-paths (apply nix-paths nixpkgs "gambit-unstable" extra-packages)))
   "ENV GAMBOPT t8,f8,-8,i8,dRr"))

(define-entry-point (make-gerbil-image (nixpkgs default-nixpkgs))
  (help: "Create image for all packages up to gerbil included"
   getopt: options/nixpkgs)
  (make-docker-image
   "mukn/gambit" "mukn/gerbil"
   (format "RUN echo gerbil-unstable ~a ; nix-channel --update ; nix-env -f '<nixpkgs>' -iA gerbil-unstable"
           (digest-paths (apply nix-paths nixpkgs "gerbil-unstable" "gambit-unstable" extra-packages)))
   "ENV GERBIL_LOADPATH /root/.nix-profile/gerbil/lib"))

(define-entry-point (make-gerbil-packages-image (nixpkgs default-nixpkgs))
  (help: "Create image for all packages including all gerbil libraries"
   getopt: options/nixpkgs)
  (make-docker-image
   "mukn/gerbil" "mukn/gerbil-packages"
   (format "RUN echo gerbilPackages-unstable ~a ; nix-channel --update ; nix-env -f '<nixpkgs>' -iA gerbilPackages-unstable"
           (digest-paths (apply nix-paths nixpkgs all-target-packages)))))

(define-entry-point (make-glow-image)
  (help: "Create image ready to roll for glow"
   getopt: [])
  (make-docker-image "mukn/gerbil-packages" "mukn/glow:devel")
  (docker-push "mukn/glow:devel"))

(define-entry-point (nix-paths (nixpkgs default-nixpkgs) . packages)
  (help: "Return the nix store paths for gerbil and gambit prerequisites plus given packages"
   getopt: options/nixpkgs+packages)
  (setenv "NIX_PATH" (format "nixpkgs=~a" nixpkgs)) ;; TODO: is this needed or overridden by -f below?
  (run-process
   coprocess: read-all-as-lines
   ["nix" "--extra-experimental-features" "nix-command" "path-info" "--recursive" "-f" nixpkgs
    (string-append
     "(with (import <nixpkgs> {}) ;"
     (apply package-expression packages) ")")]))

(define-entry-point (make-packages (nixpkgs default-nixpkgs))
  (help: "Make the nix packages and push them to the cache"
   getopt: options/nixpkgs)
  (run-command/batch (apply install-command nixpkgs all-target-packages))
  (def paths (apply nix-paths nixpkgs all-target-packages))
  (run-process/batch ["cachix" "push" "mukn" paths ...]))

(define-entry-point (all (nixpkgs default-nixpkgs))
  (help: "Rebuild all images"
   getopt: options/nixpkgs)
  (make-packages nixpkgs)
  (make-cachix-image)
  (make-pre-gambit-image nixpkgs)
  (make-gambit-image nixpkgs)
  (make-gerbil-image nixpkgs)
  (make-gerbil-packages-image nixpkgs)
  (make-glow-image))

;; We can use small, assuming we didn't update gambit or gerbil,
;; or otherwise rebased nixpkgs since we last created the mukn/gerbil image.
(define-entry-point (small (nixpkgs default-nixpkgs))
  (help: "Rebuild just gerbil-packages and glow"
   getopt: options/nixpkgs)
  (make-packages nixpkgs)
  (make-gerbil-packages-image nixpkgs)
  (make-glow-image))

(set-default-entry-point! 'all)
(backtrace-on-abort? #f)
(define-multicall-main)


;; To make a release:
;; pushtag () { for i ; do docker tag mukn/glow mukn/glow:$i ; docker push mukn/glow:$i ; done; }
;; pushtag latest devel
