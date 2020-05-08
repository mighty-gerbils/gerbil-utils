#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Make docker images for Gerbil, etc.

(import
  :gerbil/gambit/ports
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/misc/process
  :std/srfi/13 :std/sugar :std/pregexp
  :clan/utils/base :clan/utils/basic-parsers :clan/utils/multicall
  :clan/utils/timestamp :clan/utils/files)

;; Initialize paths from the environment
(def here (path-directory (path-normalize (this-source-file))))
(def (parent dir) (path-normalize (path-expand ".." dir)))
(def gerbil-utils (parent here))
(def checkouts-dir (parent gerbil-utils))

(import :clan/utils/debug)

;; We assume nixpkgs is likely be a git checkout.
;; But a full nixpkgs checkout will take 2GB of disk space for no reason.
;; So we recommend you point nixpkgs at a worktree...

(def (build-image nixpkgs_)
  (def nixpkgs (or nixpkgs_ (path-expand "nixpkgs" checkouts-dir)))
  (def version
    (with-catch false (cut run-process '("git" "describe" "--tags" "--always") directory: nixpkgs)))
  (def version-file (path-expand ".git-worktree" nixpkgs))
   (DBG foo: here gerbil-utils checkouts-dir version version-file)
  (clobber-file version-file version)
  (run-process ["nix-build" "gerbil-docker-layered.nix"]
               stdin-redirection: #f
               stdout-redirection: #f
               directory: here)
  (run-process ["docker" "load" "-i" "./result"]
               stdin-redirection: #f
               stdout-redirection: #f
               directory: here)
  (run-process ["docker" "build" "-t" "fahree/gerbil-utils" "-f" "scripts/Dockerfile" "."]
               stdin-redirection: #f
               stdout-redirection: #f
               directory: gerbil-utils)
  (run-process ["docker" "push" "fahree/gerbil-utils"]
               stdin-redirection: #f
               stdout-redirection: #f)
  (run-process ["docker" "push" "fahree/gerbil-nix"]
               stdin-redirection: #f
               stdout-redirection: #f))

(define-entry-point (make-gerbil-docker-image . arguments)
  "Create a Docker image for Gerbil"
  (def gopt
    (getopt
     (option 'nixpkgs "-N" "--nixpkgs" default: #f
             help: "nixpkgs directory (default: sibling to this checkout)")
     (option 'stable "-S" "--stable" default: #f
             help: "update stable recipes instead of unstable (default: unstable)")))
  (try
   (let ((opt (getopt-parse gopt arguments)))
     (defrule {symbol} (hash-get opt 'symbol))
     (when {stable} (error "Stable not implemented yet"))
     (build-image {nixpkgs}))
   (catch (getopt-error? exn)
     (getopt-display-help exn "update-gerbil-nix-recipe" (current-error-port))
     (exit 2))
   (catch (uncaught-exception? exn)
     (display-exception (uncaught-exception-reason exn) (current-error-port))
     (exit 2))))

(def main make-gerbil-docker-image)

;;(import :clan/utils/debug) (trace! update-recipe parse-github-argument call-with-input-process)
