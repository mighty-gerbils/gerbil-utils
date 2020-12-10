#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Make docker images for Gerbil, etc.

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports
  :std/getopt :std/misc/ports :std/misc/process :std/sugar
  :clan/base :clan/exit :clan/files :clan/multicall :clan/path :clan/source)

;; Initialize paths from the environment
(def here (path-directory (path-normalize (this-source-file))))
(def (parent dir) (path-normalize (path-expand ".." dir)))
(def gerbil-utils (parent here))
(def checkouts-dir (parent gerbil-utils))

;; We assume nixpkgs is likely be a git checkout.
;; But a full nixpkgs checkout will take 2GB of disk space for no reason.
;; So we recommend you point nixpkgs at a worktree...

(def (docker-push tag)
  (run-process ["docker" "push" tag]
               stdin-redirection: #f stdout-redirection: #f))

;; TODO: resurrect a more deterministic way to build the image, with Nix
(def (build-image-layered nixpkgs_)
  (def nixpkgs (or nixpkgs_ (path-expand "nixpkgs" checkouts-dir)))
  (def version
    (with-catch false (cut run-process '("git" "describe" "--tags" "--always") directory: nixpkgs)))
  (def version-file (path-expand "git-description" nixpkgs))
  (clobber-file version-file version)
  (run-process ["nix-build" "gerbil-docker-layered.nix"]
               directory: here stdin-redirection: #f stdout-redirection: #f)
  (delete-file version-file)
  (run-process ["docker" "load" "-i" "./result"]
               directory: here stdin-redirection: #f stdout-redirection: #f)
  (run-process ["docker" "build" "-t" "fahree/gerbil-utils" "-f" "scripts/Dockerfile" "."]
               directory: (path-parent here) stdin-redirection: #f stdout-redirection: #f)
  (docker-push "fahree/gerbil-utils:latest")
  (docker-push "fahree/gerbil-nix:latest"))

(def nixpkgs "http://github.com/muknio/nixpkgs/archive/alpha.tar.gz")

(def (build-image nixpkgs_) ;; TODO: don't ignore the argument?
  (run-process ["nix-env" "-f" nixpkgs "-iA"
                "gerbil-unstable" "gerbilPackages-unstable"]
               directory: here stdin-redirection: #f stdout-redirection: #f)
  (!>
   (run-process ["nix" "path-info" "-f" nixpkgs "-r" "gerbilPackages-unstable"])
   (cut string-split <> #\newline)
   (cut cons* "cachix" "push" "mukn" <>)
   (cut run-process <> stdin-redirection: #f stdout-redirection: #f))
  (create-directory* (path-expand "run/empty" (path-parent here)))
  (run-process ["docker" "build" "-t" "fahree/gerbil-utils" "-f" "scripts/Dockerfile.nixos" "run/empty"]
               directory: (path-parent here) stdin-redirection: #f stdout-redirection: #f)
  (docker-push "fahree/gerbil-utils:latest"))

(define-entry-point (make-gerbil-docker-image . arguments)
  "Create a Docker image for Gerbil"
  (def gopt
    (getopt
     (option 'nixpkgs "-f" "--nixpkgs" default: #f
             help: "nixpkgs directory (default: sibling to this checkout)")
     (option 'stable "-S" "--stable" default: #f
             help: "update stable recipes instead of unstable (default: unstable)")))
  (try
   (let ((opt (getopt-parse gopt arguments)))
     (defrule {symbol} (hash-get opt 'symbol))
     (when {stable} (error "Stable not implemented yet"))
     (build-image {nixpkgs})
     (silent-exit))
   (catch (getopt-error? exn)
     (getopt-display-help exn "update-gerbil-nix-recipe" (current-error-port))
     (exit 2))
   (catch (uncaught-exception? exn)
     (display-exception (uncaught-exception-reason exn) (current-error-port))
     (exit 2))))

(def main make-gerbil-docker-image)
