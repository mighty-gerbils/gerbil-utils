#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Automatically update gerbil and gambit recipes for nixpkgs from source.
;; TODO: also automatically make a new commit to nixpkgs (and let the user use git rebase -i
;; to merge the extra ones together), or somehow automatically detect which can be amended,
;; and properly edit their messages, based on what was or wasn't already upstreamed.

(import
  :gerbil/gambit/ports
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/srfi/13 :std/sugar :std/pregexp
  :clan/utils/base :clan/utils/basic-parsers :clan/utils/multicall
  :clan/utils/timestamp :clan/utils/files)

;; Initialize paths from the environment
(defonce (default-checkouts-dir)
  (path-normalize (path-expand "../.." (path-directory (this-source-file)))))

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

(def (parse-github-argument arg)
  (defvalues (owner/repo branch) (separate-string-suffix #\@ arg))
  (defvalues (owner repo) (separate-string-prefix #\/ owner/repo))
  (values owner repo branch))

(def (pregexp-string-replacer prefix old suffix new)
  (cut pregexp-replace
       (string-append "(" prefix ")" old "(" suffix ")") <>
       ;; NB: \$ is a regexp for an empty string, which prevents interference
       ;; if the substring starts with a digit, e.g. 0 causing \10 to be interpreted as 10th match.
       ;; Thanks to vyzo for debugging this issue!!!
       (string-append "\\1\\$" new "\\2")))

(def (update-recipe
      name: name
      github: github
      recipe-path: recipe-path
      checkouts-dir: (checkouts-dir #f)
      source-dir: (source-dir_ #f)
      nixpkgs-dir: (nixpkgs-dir_ #f)
      stable: (stable #f))

  (def (defaultize dir reponame)
    (path-expand (or dir reponame) (or checkouts-dir (default-checkouts-dir))))
  (def source-dir (defaultize source-dir_ name))
  (def nixpkgs-dir (defaultize nixpkgs-dir_ "nixpkgs"))
  (defvalues (gh-owner gh-repo git-tree) (parse-github-argument github))
  (def repo-url (format "https://github.com/~a/~a" gh-owner gh-repo))
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
      (pregexp-string-replacer "    owner = \"" "[-.0-9A-Za-z]+" "\";" gh-owner)
      (pregexp-string-replacer "    repo = \"" "[-.0-9A-Za-z]+" "\";" gh-repo)
      (pregexp-string-replacer "    sha256 = \"" "[0-9a-z]+" "\";" nix-source-hash)])))

(def (recipe-path lang file)
  (format "pkgs/development/compilers/~a/~a.nix" lang file))

(define-entry-point (update-gerbil-nix-recipe . arguments)
  "Update Gerbil (and Gambit) Nix recipies"
  (def gopt
    (getopt
     (option 'checkouts-dir "-C" "--checkouts" default: #f
             help: "parent directory to git checkouts")
     (flag 'stable "-S" "--stable"
             help: "update stable recipes instead of unstable")
     (flag 'gambit-off "-o" "--gambit-off"
             help: "skip gambit update")
     (option 'gambit-github "-g" "--gambit-gh" default: #f
             help: "github owner/repo[@tree] for gambit")
     (option 'gambit-dir "-d" "--gambit-dir" default: #f
             help: "git checkout directory for gambit")
     (flag 'gerbil-off "-O" "--gerbil-off"
             help: "skip gerbil update")
     (option 'gerbil-github "-G" "--gerbil-gh" default: #f
             help: "github owner/repo[@tree] for gerbil")
     (option 'gerbil-dir "-D" "--gerbil-dir" default: #f
             help: "git checkout directory for gerbil")
     (option 'gerbil-dir "-U" "--gerbil-utils-dir" default: #f
             help: "git checkout directory for gerbil-utils")
     (option 'gerbil-dir "-Y" "--gerbil-crypto-dir" default: #f
             help: "git checkout directory for gerbil-crypto")
     (option 'gerbil-dir "-E" "--gerbil-ethereum-dir" default: #f
             help: "git checkout directory for gerbil-ethereum")))
  (try
   (let ((opt (getopt-parse gopt arguments)))
     (defrule {symbol} (hash-get opt 'symbol))
     (unless {gambit-off}
       (update-recipe
        name: "gambit"
        github: (or {gambit-github} "feeley/gambit")
        recipe-path: (recipe-path "gambit" (if {stable} "default" "unstable"))
        checkouts-dir: {checkouts-dir}
        source-dir: {gambit-dir}
        nixpkgs-dir: {nixpkgs-dir}
        stable: {stable}))
     (unless {gerbil-off}
       (update-recipe
        name: "gerbil"
        github: (or {gerbil-github} "vyzo/gerbil")
        recipe-path: (recipe-path "gerbil" (if {stable} "default" "unstable"))
        checkouts-dir: {checkouts-dir}
        source-dir: {gerbil-dir}
        nixpkgs-dir: {nixpkgs-dir}
        stable: {stable}))
     (unless (or {gerbil-off} {stable})
       (update-recipe
        name: "gerbil-utils"
        github: "fare/gerbil-utils"
        recipe-path: (recipe-path "gerbil" "gerbil-utils")
        checkouts-dir: {checkouts-dir}
        source-dir: {gerbil-utils-dir}
        nixpkgs-dir: {nixpkgs-dir}
        stable: {stable}))
     (unless (or {gerbil-off} {stable})
       (update-recipe
        name: "gerbil-crypto"
        github: "fare/gerbil-crypto"
        recipe-path: (recipe-path "gerbil" "gerbil-crypto")
        checkouts-dir: {checkouts-dir}
        source-dir: {gerbil-crypto-dir}
        nixpkgs-dir: {nixpkgs-dir}
        stable: {stable}))
     (unless (or {gerbil-off} {stable})
       (update-recipe
        name: "gerbil-ethereum"
        github: "fare/gerbil-ethereum"
        recipe-path: (recipe-path "gerbil" "gerbil-ethereum")
        checkouts-dir: {checkouts-dir}
        source-dir: {gerbil-ethereum-dir}
        nixpkgs-dir: {nixpkgs-dir}
        stable: {stable})))
   (catch (getopt-error? exn)
     (getopt-display-help exn "update-gerbil-nix-recipe" (current-error-port))
     (exit 2))
   (catch (uncaught-exception? exn)
     (display-exception (uncaught-exception-reason exn) (current-error-port))
     (exit 2))))

(def main update-gerbil-nix-recipe)

;;(import :clan/utils/debug) (trace! update-recipe parse-github-argument call-with-input-process)
