;;; Various git commands used in CI

(export #t)

(import
  :gerbil/gambit
  :std/format
  :std/iter
  :std/misc/list
  :std/misc/path
  :std/misc/ports
  :std/misc/process
  :std/misc/string
  :std/net/request
  :std/pregexp
  :std/sort
  :std/source
  :std/srfi/1
  :std/sugar
  ./base ./filesystem ./io ./list
  ./path-config ./ports ./rpm-versioning ./syntax)

;; TODO: move that to another file?
;; TODO: support doing it in another directory?
(def (gerbil.pkg)
  (with-catch false (lambda () (call-with-input-file "gerbil.pkg" read))))

(def (git-origin-repo)
  (or (pgetq repo: (gerbil.pkg)) "origin"))

(def (git-origin-branch)
  (or (pgetq branch: (gerbil.pkg)) "master"))

;; Base tag for our git version description
;; Intended use: the commit that bumps this tag shall also the only that releases said base version.
(def (git-base-tag)
  (pgetq base-tag: (gerbil.pkg)))

(def (git-merge-base . commitishs)
  (run-process ["git" "merge-base" . commitishs] coprocess: read-line))

(def (git-shallow depth: (depth 1) . args)
  (run-process/batch ["git" "fetch" "--depth" (object->string depth) . args]))

(def (git-unshallow)
  (run-process/batch ["git" "fetch" "--unshallow"]))

(def (git-remote-tags)
  (run-process ["git" "ls-remote" "--sort=-v:refname" "-t" "origin"] coprocess: read-all-as-lines))

(def (normalize-git-url url)
  (cond
   ((pregexp-match "^https://(?:[-_.0-9A-Za-z]+(?:[:][-_.0-9A-Za-z]+)?@)?([-_.0-9A-Za-z]+)/(.*?)(?:[.]git/?)?$" url) =>
    (match <> ([_ host path] (format "https://~a/~a" host path))))
   ((pregexp-match "^ssh://(?:[-_.0-9A-Za-z]+(?:[:][-_.0-9A-Za-z]+)?@)?([-_.0-9A-Za-z]+)/(.*?)(?:[.]git/?)?$" url) =>
    (match <> ([_ host path] (format "https://~a/~a" host path))))
   ((pregexp-match "^(?:[-_.0-9A-Za-z]+(?:[:][-_.0-9A-Za-z]+)?@)?([-_.0-9A-Za-z]+):(.*?)(?:[.]git/?)?$" url) =>
    (match <> ([_ host path] (format "https://~a/~a" host path))))
   (else (error "Not a recognized git url" url))))

(def (git-repo-url repo)
  (if (string-index repo ":") repo
      (run-process ["git" "config" "--get" (format "remote.~a.url" repo)] coprocess: read-line)))

(def (number-of-commits-from-gitlab repo from to)
  (def gitlab-url (!> repo git-repo-url normalize-git-url))
  (def url (format "~a/-/compare/~a...~a" gitlab-url from to))
  (with-catch false
   (cut call-with-input-u8vector
    (request-content (http-get url))
    (lambda (port)
      (let/cc return
        (for ((line (in-input-lines port)))
          (match (pregexp-match "^Commits [(]([0-9]+)[)]$" line)
            ([_ s] (close-port port) (return (string->number s)))
            (#f (void))))
        #f)))))

(def (git-commit-hash . args)
  (car (run-process ["git" "log" "-1" "--format=%H" . args] coprocess: read-all-as-lines)))

(def (git-remote-head (origin (git-origin-repo)) (branch (git-origin-branch)))
  (def line (car (run-process ["git" "ls-remote" (git-origin-repo) branch] coprocess: read-all-as-lines)))
  (cadr (pregexp-match "^([0-9a-f]+)\\t" line)))

;; TODO: Write a function that can query the remote git/gitlab/github server
;; in case this is to run on a test runner that has insufficient depth to
;; include the reference branch.
(def (git-up-to-date-with-branch?
      (commit (git-commit-hash)) (origin (git-origin-repo)) (branch (git-origin-branch)))
  (def head (git-remote-head origin branch))
  (equal? head (with-catch false (cut git-merge-base commit head))))

;; TODO: support more than vN(.N)* as "versions"
(def (tag-versions-from-git-remote-line line)
  (match (pregexp-match "^[0-9a-f]+\\trefs/tags/v([0-9]+(?:.[0-9]+)*)$" line)
    ([_ s] [s])
    (#f [])))

(def (git-remote-version-tags)
  (append-map tag-versions-from-git-remote-line (git-remote-tags)))

;; TODO: To support security patches on a previously-released version,
;; maybe the reference version used should be stashed in the gerbil.pkg
;; instead of always being the latest one? This way, we don't need to rely on heuristics
;; to autodetect a "latest" tag.
(def (git-latest-version)
  (def local-tags (run-process ["git" "tag"] coprocess: read-all-as-lines))
  (def tags
    (if (null? local-tags)
      (git-remote-version-tags)
      local-tags))
  (extremum<-list rpm-version> tags))

(def (git-describe commit: (commit #f))
  (nest
    (let* ((commit-hash
            (apply git-commit-hash (when/list commit [commit])))
           (local-description
            (car (run-process ["git" "describe" "--tags" "--always"] coprocess: read-all-as-lines)))))
    (if (not (pregexp-match "^[0-9a-f]+$" local-description))
      local-description)
    ;; Local description contains no tag: try harder
    ;; TODO: Support more than gitlab as the remote server.
    (let* ((tag (git-base-tag))
           (commits
            (and tag (number-of-commits-from-gitlab (git-origin-repo) tag commit-hash)))))
    (if commits
      (format "~a-~d-g~a" tag commits (substring commit-hash 0 7)))
    local-description))

(def (git-commit-date . args)
  (with-catch false
              (cut car (run-process ["git" "log" "-1" "--pretty=%ad" "--date=short" args ...]
                                    coprocess: read-all-as-lines))))

;; Parse a git description as returned by git describe --tags into a list-encoded tuple of:
;; the top tag in the commit, the number of commits since that tag, and the 7-hex-char commit hash
;; if any was provided (if none, then use the tag).
;; : (Tuple String Nat (Or String #f)) <- String
(def (parse-git-description description)
  (match (pregexp-match "^(.*)-([0-9]+)-g([0-9a-f]{7})$" description)
    ([_ tag commits hash]
     [tag (string->number commits) hash])
    (else
     [description 0 #f])))

;; Update the version file from git
;; name: name of the project, e.g. "GNU Hello" (mandatory)
;; repo: where is the git repository compared to the current ./build.ss directory?
;;   use #f or "." if same directory, "..", "../..", "../../.." or such if above.
;;   (optional, default: #f).
;; path: which file will contain the version?
;;   (optional, default: "version.ss").
;; NB: You need to have at least one git tag, as created with e.g. git tag v0.0
(def (update-version-from-git
      name: name
      repo: (repo #f)
      path: (path_ #f)
      deps: (deps '()))
  (def path (or path_ "version.ss"))
  (def git-version
    (and (file-exists? (path-expand ".git" (or repo ".")))
         (git-describe)))
  (def git-date
    (and git-version (git-commit-date)))
  (def version-text
    (and git-date
         (format "~a\n~s ;; ~a\n"
                 `(import :clan/versioning ,@(map (cut format ":~a/version" <>) deps))
                 `(register-software ,name ,git-version) git-date)))
  (def previous-version-text
    (and version-text ;; no need to compute it if no current version to replace it with
         (file-exists? path)
         (read-file-string path)))
  (if (and version-text (not (equal? version-text previous-version-text)))
    (call-with-output-file [path: path create: 'maybe append: #f truncate: #t]
      (cut display version-text <>))))
