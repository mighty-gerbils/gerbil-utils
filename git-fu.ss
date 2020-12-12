;;; Various git commands used in CI

(export #t)

(import
  :gerbil/gambit/ports
  :std/format :std/iter
  :std/misc/list :std/misc/ports :std/misc/process :std/misc/string
  :std/net/request
  :std/pregexp :std/sort :std/srfi/1 :std/sugar
  ./base ./exit ./filesystem ./io ./list
  ./path ./path-config ./ports ./rpm-versioning ./source ./syntax ./with-id)

;; TODO: move that to another file?
;; TODO: support doing it in another directory?
(def (gerbil.pkg)
  (with-catch false (lambda () (call-with-input-file "gerbil.pkg" read))))

(def (git-origin-repo)
  (or (pgetq repo: (gerbil.pkg)) "origin"))

(def (git-origin-branch)
  (or (pgetq branch: (gerbil.pkg)) "master"))

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
   ((pregexp-match "^https?://" url) url)
   ((pregexp-match "^(?:ssh://)?(?:git@)?([-_.a-z0-9]+):(.*?)(?:[.]git)?$" url) =>
    (match <> ([_ host path] (format "https://~a/~a" host path))))
   (else (error "Not a recognized git url" url))))

(def (git-repo-url repo)
  (if (string-index repo ":") repo
      (run-process ["git" "config" "--get" (format "remote.~a.url" repo)] coprocess: read-line)))

(def (number-of-commits-from-gitlab repo from to)
  (def gitlab-url (!> repo git-repo-url normalize-git-url))
  (def url (format "~a/-/compare/~a...~a" gitlab-url from to))
  (call-with-input-u8vector
   (request-content (http-get url))
   (lambda (port)
     (let/cc return
       (for ((line (in-input-lines port)))
         (match (pregexp-match "^Commits [(]([0-9]+)[)]$" line)
           ([_ s] (close-port port) (return (string->number s)))
           (#f (void))))
       #f))))

(def (git-commit-hash . args)
  (run-process ["git" "log" "-1" "--format=%H" . args] coprocess: read-line))

(def (git-remote-head (origin (git-origin-repo)) (branch (git-origin-branch)))
  (def line (run-process ["git" "ls-remote" (git-origin-repo) branch] coprocess: read-line))
  (cadr (pregexp-match "^([0-9a-f]+)\\t" line)))

(def (git-up-to-date-with-branch?
      (commit (git-commit-hash)) (origin (git-origin-repo)) (branch (git-origin-branch)))
  (def head (git-remote-head origin branch))
  (equal? head (with-catch false (cut git-merge-base commit head))))

(def (tag-versions-from-git-remote-line line)
  (match (pregexp-match "^[0-9a-f]+\\trefs/tags/v([0-9]+(?:.[0-9]+)*)$" line)
    ([_ s] [s])
    (#f [])))

(def (git-remote-version-tags)
  (append-map tag-versions-from-git-remote-line (git-remote-tags)))

(def (git-remote-latest-version)
  (string-append "v" (extremum<-list rpm-version> (git-remote-version-tags))))

(def (git-describe commit: (commit #f))
  (let/cc return
    (def hash
      (apply git-commit-hash (when/list commit [commit])))
    (def local-description
      (run-process ["git" "describe" "--tags" "--always"] coprocess: read-line))
    (unless (pregexp-match "^[0-9a-f]+$" local-description)
      (return local-description))
    ;; Local description contains no tag: try harder
    (def tag
      (git-remote-latest-version))
    (def commits
      (number-of-commits-from-gitlab (git-origin-repo) tag hash))
    (when commits
      (return (format "~a-~d-g~a" tag commits (substring hash 0 7))))
    local-description))

(def (process-output-line command)
  (with-catch false (cut run-process command coprocess: read-line)))

(def (git-date . args)
  (process-output-line ["git" "log" "-1" "--pretty=%ad" "--date=short" args ...]))
