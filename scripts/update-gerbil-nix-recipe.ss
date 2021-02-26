#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Automatically update gerbil and gambit recipes for nixpkgs from source.
;; TODO: also automatically make a new commit to nixpkgs (and let the user use git rebase -i
;; to merge the extra ones together), or somehow automatically detect which can be amended,
;; and properly edit their messages, based on what was or wasn't already upstreamed.

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/srfi/13 :std/sugar :std/pregexp
  :clan/base :clan/basic-parsers :clan/files
  :clan/multicall :clan/path :clan/source :clan/timestamp)

;; Initialize paths from the environment
(defonce (default-checkouts-dir)
  (path-maybe-normalize (path-expand "../.." (path-directory (this-source-file)))))

(def (separate-string-prefix separator string)
  (if string
    (let (pos (string-index string separator))
      (if pos
        (values (substring string 0 pos) (substring string (1+ pos) (string-length string)))
        (values #f string)))
    (values #f #f)))

(def (separate-string-suffix separator string)
  (if string
    (let (pos (string-index-right string separator))
      (if pos
        (values (substring string 0 pos) (substring string (1+ pos) (string-length string)))
        (values string #f)))
    (values #f #f)))

(def (parse-repo-argument arg)
  (match (pregexp-match "^(?:(github|gitlab)(?:.com)?/)?([-_.a-zA-Z0-9]+)/([-_.a-zA-Z0-9]+)(?:@([-_.a-zA-Z0-9]+))?$" arg)
    ([_ site_ owner repo-name branch]
     (let (site (or site_ "github"))
       (values (format "https://~a.com/~a/~a" site owner repo-name) site owner repo-name branch)))
    (#f
     (error "Can't parse repo" arg))))

(def (pregexp-string-replacer prefix old suffix new)
  (cut pregexp-replace
       (string-append "(" prefix ")" old "(" suffix ")") <>
       ;; NB: \$ is a regexp for an empty string, which prevents interference
       ;; if the substring starts with a digit, e.g. 0 causing \10 to be interpreted as 10th match.
       ;; Thanks to vyzo for debugging this issue!!!
       (string-append "\\1\\$" new "\\2")))

(def (update-recipe
      name: name
      repo: repo
      recipe-path: recipe-path
      checkouts-dir: (checkouts-dir #f)
      source-dir: (source-dir_ #f)
      nixpkgs-dir: (nixpkgs-dir_ #f)
      stable: (stable #f))

  (def (defaultize dir reponame)
    (path-expand (or dir reponame) (or checkouts-dir (default-checkouts-dir))))
  (def source-dir (defaultize source-dir_ name))
  (def nixpkgs-dir (defaultize nixpkgs-dir_ "nixpkgs"))
  (defvalues (repo-url site owner repo-name git-tree) (parse-repo-argument repo))
  (def recipe-file (path-expand recipe-path nixpkgs-dir))

  ;; Update source directory via git fetch
  (call-with-input-process
   [path: "git" arguments: ["fetch" "--tags" repo-url (when/list git-tree [git-tree]) ...]
          directory: source-dir show-console: #f]
   read-all-as-string)

  ;; Extract top commit ID (SHA1 as 40 hex character string)
  (def latest-commit-hash
    (call-with-input-process
     [path: "git" arguments: ["log" "-1" "--pretty=oneline" "FETCH_HEAD"]
            directory: source-dir show-console: #f]
     (λ (port) (pregexp-replace "^([0-9a-z]+) .*$" (read-line port) "\\1"))))

  ;; Extract version string
  (def git-version
    (call-with-input-process
     [path: "git" arguments: ["describe" "--tags" "--always" "FETCH_HEAD"]
            directory: source-dir show-console: #f]
     ;; strip the leading v: nix can't find the version if there is a v
     (λ (port) (pregexp-replace "^v(.*)$" (read-line port) "\\1"))))

  (def commit-unix-time
    (call-with-input-process
     [path: "git" arguments: ["show" "-s" "--pretty=format:%ct" latest-commit-hash]
            directory: source-dir show-console: #f]
     expect-natural))

  (def package-date (string<-unix-time commit-unix-time "~Y-~m-~d"))
  (def package-version (if stable package-date (string-append "unstable-" package-date)))

  ;; Extract the new hash using nix-prefetch-git (sha256 as a 52(?) character base-36 string).
  (def nix-source-hash
    (call-with-input-process
     [path: "nix-prefetch-git"
            arguments: ["--url" repo-url "--rev" latest-commit-hash]
            show-console: #f stderr-redirection: #f]
     (λ (port)
       (cadr (pregexp-match
              "\"sha256\": \"([0-9a-z]+)\"," (read-all-as-string port))))))

  ;; Insert new commit ID in the recipe file
  (maybe-replace-file
   recipe-file
   (apply compose
     [(if stable
        [(pregexp-string-replacer "  version = \"" "[-.0-9A-Za-z]+" "\";" git-version)]
        [(pregexp-string-replacer "  version = \"" "[-.0-9A-Za-z]+" "\";" package-version)
         (pregexp-string-replacer " git-version = \"" "[-.0-9A-Za-z]+" "\";" git-version)
         (pregexp-string-replacer "    rev = \"" "[0-9a-f]+" "\";" latest-commit-hash)])...
      (pregexp-string-replacer "    owner = \"" "[-.0-9A-Za-z]+" "\";" owner)
      (pregexp-string-replacer "    repo = \"" "[-.0-9A-Za-z]+" "\";" repo-name)
      (pregexp-string-replacer "    sha256 = \"" "[0-9a-z]+" "\";" nix-source-hash)])))

(def (recipe-path lang file)
  (format "pkgs/development/compilers/~a/~a.nix" lang file))

(define-entry-point (update-gerbil-nix-recipe
                     checkouts-dir: (checkouts-dir #f)
                     nixpkgs-dir: (nixpkgs-dir #f)
                     stable: (stable #f)
                     gambit-off: (gambit-off #f)
                     gambit-repo: (gambit-repo #f)
                     gambit-dir: (gambit-dir #f)
                     gerbil-off: (gerbil-off #f)
                     gerbil-repo: (gerbil-repo #f)
                     gerbil-dir: (gerbil-dir #f)
                     gerbil-utils-dir: (gerbil-utils-dir #f)
                     gerbil-crypto-dir: (gerbil-crypto-dir #f)
                     gerbil-poo-dir: (gerbil-poo-dir #f)
                     gerbil-persist-dir: (gerbil-persist-dir #f)
                     gerbil-ethereum-dir: (gerbil-ethereum-dir #f)
                     glow-dir: (glow-dir #f)
                     glow-repo: (glow-repo #f)
                     smug-gerbil-dir: (smug-gerbil-dir #f)
                     gerbil-libp2p-dir: (gerbil-libp2p-dir #f))
  (help: "Update Gerbil (and Gambit) Nix recipies"
   getopt: [(option 'checkouts-dir "-C" "--checkouts" default: #f
                    help: "parent directory to git checkouts")
            (flag 'nixpkgs-dir "-N" "--nixpkgs"
                  help: "path or url to nixpkgs")
            (flag 'stable "-S" "--stable"
                  help: "update stable recipes instead of unstable")
            (flag 'gambit-off "-o" "--gambit-off"
                  help: "skip gambit update")
            (option 'gambit-repo "-g" "--gambit-repo" default: #f
                    help: "github owner/repo[@tree] for gambit")
            (option 'gambit-dir "-d" "--gambit-dir" default: #f
                    help: "git checkout directory for gambit")
            (flag 'gerbil-off "-O" "--gerbil-off"
                  help: "skip gerbil update")
            (option 'gerbil-repo "-G" "--gerbil-repo" default: #f
                    help: "github owner/repo[@tree] for gerbil")
            (option 'gerbil-dir "-D" "--gerbil-dir" default: #f
                    help: "git checkout directory for gerbil")
            (option 'gerbil-utils-dir "-U" "--gerbil-utils-dir" default: #f
                    help: "git checkout directory for gerbil-utils")
            (option 'gerbil-poo-dir "-p" "--gerbil-poo-dir" default: #f
                    help: "git checkout directory for gerbil-poo")
            (option 'gerbil-crypto-dir "-Y" "--gerbil-crypto-dir" default: #f
                    help: "git checkout directory for gerbil-crypto")
            (option 'gerbil-persist-dir "-P" "--gerbil-persist-dir" default: #f
                    help: "git checkout directory for gerbil-persist")
            (option 'gerbil-ethereum-dir "-E" "--gerbil-ethereum-dir" default: #f
                    help: "git checkout directory for gerbil-ethereum")
            (option 'glow-dir "-W" "--glow-dir" default: #f
                    help: "git checkout directory for glow")
            (option 'glow-repo "-w" "--glow-repo" default: #f
                    help: "git repo for glow")
            (option 'smug-gerbil-dir "-s" "--smug-gerbil-dir" default: #f
                    help: "git checkout directory for smug-gerbil")
            (option 'gerbil-libp2p-dir "-2" "--gerbil-libp2p-dir" default: #f
                    help: "git checkout directory for gerbil-libp2p")])
  (try
   (unless gambit-off
     (update-recipe
      name: "gambit"
      repo: (or gambit-repo "feeley/gambit")
      recipe-path: (recipe-path "gambit" (if stable "default" "unstable"))
      checkouts-dir: checkouts-dir
      source-dir: gambit-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless gerbil-off
     (update-recipe
      name: "gerbil"
      repo: (or gerbil-repo "vyzo/gerbil")
      recipe-path: (recipe-path "gerbil" (if stable "default" "unstable"))
      checkouts-dir: checkouts-dir
      source-dir: gerbil-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-utils"
      repo: "fare/gerbil-utils"
      recipe-path: (recipe-path "gerbil" "gerbil-utils")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-utils-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-crypto"
      repo: "fare/gerbil-crypto"
      recipe-path: (recipe-path "gerbil" "gerbil-crypto")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-crypto-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-poo"
      repo: "fare/gerbil-poo"
      recipe-path: (recipe-path "gerbil" "gerbil-poo")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-crypto-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-persist"
      repo: "fare/gerbil-persist"
      recipe-path: (recipe-path "gerbil" "gerbil-persist")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-persist-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-ethereum"
      repo: "fare/gerbil-ethereum"
      recipe-path: (recipe-path "gerbil" "gerbil-ethereum")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-ethereum-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "smug-gerbil"
      repo: "drewc/smug-gerbil"
      recipe-path: (recipe-path "gerbil" "smug-gerbil")
      checkouts-dir: checkouts-dir
      source-dir: smug-gerbil-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-libp2p"
      repo: "github/vyzo/gerbil-libp2p"
      recipe-path: (recipe-path "gerbil" "gerbil-libp2p")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-libp2p-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-libp2p"
      repo: "github/vyzo/gerbil-libp2p"
      recipe-path: (recipe-path "gerbil" "gerbil-libp2p")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-libp2p-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "glow"
      repo: (or glow-repo "gitlab/mukn/glow")
      recipe-path: (recipe-path "gerbil" "glow-lang")
      checkouts-dir: checkouts-dir
      source-dir: glow-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable))
   (catch (getopt-error? exn)
     (getopt-display-help exn "update-gerbil-nix-recipe" (current-error-port))
     (exit 2))
   (catch (uncaught-exception? exn)
     (display-exception (uncaught-exception-reason exn) (current-error-port))
     (exit 2))))

(def (main . args) (apply call-entry-point 'update-gerbil-nix-recipe args))
