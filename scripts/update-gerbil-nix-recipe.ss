#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Automatically update gerbil/default.nix with the latest commit from origin/master
;; TODO: also automatically make a new commit to nixpkgs and/or amend the top one,
;; if the top one is a gerbil commit already and wasn't upstreamed yet and.
;; TODO: do the same for gambit -- maybe in same script?
;; or factoring common things together in clan/nix/something ?

(import
  :gerbil/gambit/ports
  :std/format :std/misc/ports :std/sugar :std/pregexp
  :clan/utils/base :clan/utils/basic-parsers :clan/utils/date :clan/utils/files)

;; Initialize paths from the environment
(def fricfrac-src (getenv "FRICFRAC_SRC"))
(def source-repos (path-normalize (path-expand ".." fricfrac-src)))
(def gambit-src (path-expand "gambit" source-repos))
(def gerbil-src (path-expand "gerbil" source-repos))
(def nixpkgs-src (path-expand "nixpkgs" source-repos))

(def (update-unstable-recipe
      source-url: source-url
      source-directory: source-directory
      package-path: package-path)

  ;; Update source directory via git fetch
  (call-with-input-process
   [path: "git" arguments: ["fetch"]
          directory: source-directory show-console: #f]
   read-all-as-string)

  ;; Extract top commit ID (SHA1 as 40 hex character string)
  (def latest-commit-hash
    (call-with-input-process
     [path: "git" arguments: ["log" "-1" "--pretty=oneline" "origin/master"]
            directory: source-directory show-console: #f]
     (λ (port) (pregexp-replace "^([0-9a-z]+) .*$" (read-line port) "\\1"))))

  ;; Extract version string
  (def git-version
    (call-with-input-process
     [path: "git" arguments: ["describe" "--tags" "origin/master"]
            directory: source-directory show-console: #f]
     ;; strip the leading v: nix can't find the version if there is a v
     (λ (port) (pregexp-replace "^v(.*)$" (read-line port) "\\1"))))

  (def commit-unix-time
    (call-with-input-process
     [path: "git" arguments: ["show" "-s" "--pretty=format:%ct" latest-commit-hash]
            directory: source-directory show-console: #f]
     expect-natural))

  (def package-version
    (string<-unix-time commit-unix-time "unstable-~Y-~m-~d"))

  ;; Extract the new hash using nix-prefetch-git (sha256 as a 52(?) character base-36 string).
  (def nix-source-hash
    (call-with-input-process
     [path: "nix-prefetch-git"
            arguments: ["--url" source-url "--rev" latest-commit-hash]
            show-console: #f stderr-redirection: #f]
     (λ (port)
       (cadr (pregexp-match
              "\"sha256\": \"([0-9a-z]+)\"," (read-all-as-string port))))))

  (def recipe-file (string-append nixpkgs-src "/" package-path "/unstable.nix"))

  (def (pregexp-string-replacer prefix old suffix new)
    (cut pregexp-replace
      (string-append "(" prefix ")" old "(" suffix ")") <>
      ;; NB: \$ is a regexp for an empty string, which prevents interference
      ;; if the substring starts with a digit, e.g. 0 causing \10 to be interpreted as 10th match.
      (string-append "\\1\\$" new "\\2")))

  ;; Insert new commit ID in the recipe file
  (maybe-replace-file
   recipe-file
   (compose
    (pregexp-string-replacer "  version = \"" "[-.0-9A-Za-z]+" "\";" package-version)
    (pregexp-string-replacer  " git-version = \"" "[-.0-9A-Za-z]+" "\";" git-version)
    (pregexp-string-replacer "    rev = \"" "[0-9a-f]+" "\";" latest-commit-hash)
    (pregexp-string-replacer "    sha256 = \"" "[0-9a-z]+" "\";" nix-source-hash))))

(def (update-gerbil-unstable)
  (update-unstable-recipe
   source-directory: gerbil-src
   package-path: "pkgs/development/compilers/gerbil"
   source-url: "https://github.com/vyzo/gerbil"))

(def (update-gambit-unstable)
  (update-unstable-recipe
   source-directory: gambit-src
   package-path: "pkgs/development/compilers/gambit"
   source-url: "https://github.com/feeley/gambit"))

(update-gambit-unstable)
(update-gerbil-unstable)
