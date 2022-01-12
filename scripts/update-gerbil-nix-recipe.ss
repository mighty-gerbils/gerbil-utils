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
  (path-maybe-normalize (path-expand "../.." (this-source-directory))))

(defonce (default-checkouts-by-owner-dir)
  (path-maybe-normalize (path-expand "../../.." (this-source-directory))))

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

(def (error/dir-not-found name dir sf)
  (define s (format "~a: directory not found\n  at: ~a" name dir))
  (error
    (cond
      ((not (pair? sf))       s)
      ((not (pair? (cdr sf))) (format "~a\n  suggested fix: ~a" s (car sf)))
      (else                   (format "~a\n  suggested fixes: ~a" s sf)))))

;; cmdopts contains the command-line options that could be changed/added to either
;;   fix the source-dir directly, or stop it from attempting to update this recipe
(def (update-recipe
      name: name
      repo: repo
      recipe-path: recipe-path
      checkouts-dir: (checkouts-dir_ #f)
      source-dir: (source-dir_ #f)
      nixpkgs-dir: (nixpkgs-dir_ #f)
      stable: (stable #f)
      cmdopts: (cmdopts []))

  (def checkouts-dir (or checkouts-dir_ (default-checkouts-dir)))
  (unless (file-exists? checkouts-dir)
    (error/dir-not-found "checkouts-dir" checkouts-dir ["-C" "--checkouts"]))

  (def (defaultize dir owner reponame)
    (cond (dir (path-expand dir checkouts-dir))
          (else
           (let ((d1 (path-expand reponame checkouts-dir))
                 (d2 (path-expand reponame (path-expand owner (default-checkouts-by-owner-dir)))))
             (if (or (file-exists? d1) (not (file-exists? d2))) d1 d2)))))
  (defvalues (repo-url site owner repo-name git-tree) (parse-repo-argument repo))
  (def source-dir (defaultize source-dir_ owner repo-name))
  (def nixpkgs-dir (defaultize nixpkgs-dir_ "NixOS" "nixpkgs"))
  (def recipe-file (path-expand recipe-path nixpkgs-dir))

  (unless (file-exists? source-dir)
    (error/dir-not-found name source-dir cmdopts))

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
  (def package-ymd (string<-unix-time commit-unix-time "~Y~m~d"))
  (def package-hms (string<-unix-time commit-unix-time "~H~M~S"))

  ;; Extract the new hash using nix-prefetch-git (sha256 as a 52(?) character base-36 string).
  (def nix-source-hash
    (call-with-input-process
     [path: "nix-prefetch-git"
            arguments: ["--url" repo-url "--rev" latest-commit-hash]
            show-console: #f stderr-redirection: #f]
     (λ (port)
       (cadr (pregexp-match
              "\"sha256\": \"([0-9a-z]+)\"," (read-all-as-string port))))))

  (unless (file-exists? recipe-file)
    (error/dir-not-found "nixpkgs" nixpkgs-dir ["-N" "--nixpkgs"]))

  ;; Insert new commit ID in the recipe file
  ;; Also support other parameters used in gambit's stamp.h
  ;; See stamp: target in gambit/include/makefile.in and according nix recipe.
  (maybe-replace-file
   recipe-file
   (apply compose
     [(if stable
        [(pregexp-string-replacer "  version = \"" "[-.0-9A-Za-z]+" "\";" git-version)]
        [(pregexp-string-replacer "  version = \"" "[-.0-9A-Za-z]+" "\";" package-version)
         (pregexp-string-replacer " git-version = \"" "[-.0-9A-Za-z]+" "\";" git-version)
         (pregexp-string-replacer " stampYmd = " "[0-9]+" ";" package-ymd)
         (pregexp-string-replacer " stampHms = " "[0-9]+" ";" package-hms)
         (pregexp-string-replacer "    rev = \"" "[0-9a-f]+" "\";" latest-commit-hash)])...
      (pregexp-string-replacer "    owner = \"" "[-.0-9A-Za-z]+" "\";" owner)
      (pregexp-string-replacer "    repo = \"" "[-.0-9A-Za-z]+" "\";" repo-name)
      (pregexp-string-replacer "    sha256 = \"" "[0-9a-z]+" "\";" nix-source-hash)])))

(def (recipe-path lang file)
  (format "pkgs/development/compilers/~a/~a.nix" lang file))

(def getopt-spec
  [(flag 'help "-h" "--help"
          help: "show help")
   (option 'checkouts-dir "-C" "--checkouts"
           help: "parent directory to git checkouts")
   (flag 'nixpkgs-dir "-N" "--nixpkgs"
         help: "path or url to nixpkgs")
   (flag 'stable "-S" "--stable"
         help: "update stable recipes instead of unstable")
   (flag 'gambit-off "-o" "--gambit-off"
         help: "skip gambit update")
   (option 'gambit-repo "-g" "--gambit-repo"
           help: "github owner/repo[@tree] for gambit")
   (option 'gambit-dir "-d" "--gambit-dir"
           help: "git checkout directory for gambit")
   (flag 'gerbil-off "-O" "--gerbil-off"
         help: "skip gerbil update")
   (option 'gerbil-repo "-G" "--gerbil-repo"
           help: "github owner/repo[@tree] for gerbil")
   (option 'gerbil-dir "-D" "--gerbil-dir"
           help: "git checkout directory for gerbil")
   (option 'gerbil-utils-dir "--gerbil-utils-dir"
           help: "git checkout directory for gerbil-utils")
   (option 'gerbil-utils-repo "--gerbil-utils-repo"
           help: "git repository for gerbil-utils")
   (option 'gerbil-poo-dir "--gerbil-poo-dir"
           help: "git checkout directory for gerbil-poo")
   (option 'gerbil-poo-repo "--gerbil-poo-repo"
           help: "git repository for gerbil-poo")
   (option 'gerbil-crypto-dir "--gerbil-crypto-dir"
           help: "git checkout directory for gerbil-crypto")
   (option 'gerbil-crypto-repo "--gerbil-crypto-repo"
           help: "git repository for gerbil-crypto")
   (option 'gerbil-persist-dir "--gerbil-persist-dir"
           help: "git checkout directory for gerbil-persist")
   (option 'gerbil-persist-repo "--gerbil-persist-repo"
           help: "git repository for gerbil-persist")
   (option 'gerbil-ethereum-dir "--gerbil-ethereum-dir"
           help: "git checkout directory for gerbil-ethereum")
   (option 'gerbil-ethereum-repo "--gerbil-ethereum-repo"
           help: "git repository for gerbil-ethereum")
   (option 'glow-dir "-W" "--glow-dir"
           help: "git checkout directory for glow")
   (option 'glow-repo "-w" "--glow-repo"
           help: "git repo for glow")
   (option 'smug-gerbil-dir "--smug-gerbil-dir"
           help: "git checkout directory for smug-gerbil")
   (option 'smug-gerbil-repo "--smug-gerbil-repo"
           help: "git repository for smug-gerbil")
   (option 'gerbil-libp2p-dir "--gerbil-libp2p-dir"
           help: "git checkout directory for gerbil-libp2p")
   (option 'gerbil-libp2p-repo "--gerbil-libp2p-repo"
           help: "git repo for gerbil-libp2p")
   (option 'ftw-dir "--ftw-dir"
           help: "git checkout directory for ftw")
   (option 'ftw-repo "--ftw-repo"
           help: "git repository for ftw")])

(def program "update-gerbil-nix-recipe")

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
                     gerbil-utils-repo: (gerbil-utils-repo #f)
                     gerbil-crypto-dir: (gerbil-crypto-dir #f)
                     gerbil-crypto-repo: (gerbil-crypto-repo #f)
                     gerbil-poo-dir: (gerbil-poo-dir #f)
                     gerbil-poo-repo: (gerbil-poo-repo #f)
                     gerbil-persist-dir: (gerbil-persist-dir #f)
                     gerbil-persist-repo: (gerbil-persist-repo #f)
                     gerbil-ethereum-dir: (gerbil-ethereum-dir #f)
                     gerbil-ethereum-repo: (gerbil-ethereum-repo #f)
                     glow-dir: (glow-dir #f)
                     glow-repo: (glow-repo #f)
                     smug-gerbil-dir: (smug-gerbil-dir #f)
                     smug-gerbil-repo: (smug-gerbil-repo #f)
                     gerbil-libp2p-dir: (gerbil-libp2p-dir #f)
                     gerbil-libp2p-repo: (gerbil-libp2p-repo #f)
                     ftw-dir: (ftw-dir #f)
                     ftw-repo: (ftw-repo #f)
                     help: (help #f))
  (help: "Update Gerbil (and Gambit) Nix recipies"
   getopt: getopt-spec)
  (when help
    (getopt-display-help (apply getopt getopt-spec) program)
    (force-output)
    (exit 0))
  (try
   (unless gambit-off
     (update-recipe
      name: "gambit"
      repo: (or gambit-repo "gambit/gambit")
      recipe-path: (recipe-path "gambit" (if stable "default" "unstable"))
      checkouts-dir: checkouts-dir
      source-dir: gambit-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--gambit-repo" "--gambit-dir" "--gambit-off"]))
   (unless gerbil-off
     (update-recipe
      name: "gerbil"
      repo: (or gerbil-repo "vyzo/gerbil")
      recipe-path: (recipe-path "gerbil" (if stable "default" "unstable"))
      checkouts-dir: checkouts-dir
      source-dir: gerbil-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--gerbil-repo" "--gerbil-dir" "--gerbil-off"]))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-utils"
      repo: (or gerbil-utils-repo "fare/gerbil-utils")
      recipe-path: (recipe-path "gerbil" "gerbil-utils")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-utils-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--gerbil-utils-dir" "--gerbil-utils-repo" "--gerbil-off" "--stable"]))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-crypto"
      repo: (or gerbil-crypto-repo "fare/gerbil-crypto")
      recipe-path: (recipe-path "gerbil" "gerbil-crypto")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-crypto-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--gerbil-crypto-dir" "--gerbil-crypto-repo" "--gerbil-off" "--stable"]))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-poo"
      repo: (or gerbil-poo-repo "fare/gerbil-poo")
      recipe-path: (recipe-path "gerbil" "gerbil-poo")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-poo-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--gerbil-poo-dir" "--gerbil-poo-repo" "--gerbil-off" "--stable"]))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-persist"
      repo: (or gerbil-persist-repo "fare/gerbil-persist")
      recipe-path: (recipe-path "gerbil" "gerbil-persist")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-persist-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--gerbil-persist-dir" "--gerbil-persist-repo" "--gerbil-off" "--stable"]))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-ethereum"
      repo: (or gerbil-ethereum-repo "fare/gerbil-ethereum")
      recipe-path: (recipe-path "gerbil" "gerbil-ethereum")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-ethereum-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--gerbil-ethereum-dir" "--gerbil-ethereum-repo" "--gerbil-off" "--stable"]))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "smug-gerbil"
      repo: (or smug-gerbil-repo "drewc/smug-gerbil")
      recipe-path: (recipe-path "gerbil" "smug-gerbil")
      checkouts-dir: checkouts-dir
      source-dir: smug-gerbil-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--smug-gerbil-dir" "--smug-gerbil-repo" "--gerbil-off" "--stable"]))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "gerbil-libp2p"
      repo: (or gerbil-libp2p-repo "github/vyzo/gerbil-libp2p")
      recipe-path: (recipe-path "gerbil" "gerbil-libp2p")
      checkouts-dir: checkouts-dir
      source-dir: gerbil-libp2p-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--gerbil-libp2p-repo" "--gerbil-libp2p-dir" "--gerbil-off" "--stable"]))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "ftw"
      repo: (or ftw-repo "github/drewc/ftw")
      recipe-path: (recipe-path "gerbil" "ftw")
      checkouts-dir: checkouts-dir
      source-dir: ftw-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--ftw-repo" "--ftw-dir" "--gerbil-off" "--stable"]))
   (unless (or gerbil-off stable)
     (update-recipe
      name: "glow"
      repo: (or glow-repo "github/Glow-Lang/glow") ;;"gitlab/mukn/glow")
      recipe-path: (recipe-path "gerbil" "glow-lang")
      checkouts-dir: checkouts-dir
      source-dir: glow-dir
      nixpkgs-dir: nixpkgs-dir
      stable: stable
      cmdopts: ["--glow-repo" "--glow-dir" "--gerbil-off" "--stable"]))
   (catch (getopt-error? exn)
     (getopt-display-help (apply getopt getopt-spec) program (current-error-port))
     (exit 2))
   (catch (uncaught-exception? exn)
     (display-exception (uncaught-exception-reason exn) (current-error-port))
     (exit 2))))

(def (main . args) (apply call-entry-point 'update-gerbil-nix-recipe args))
