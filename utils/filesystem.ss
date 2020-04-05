;; -*- Gerbil -*-
;;;; Utilities to interface the filesystem

(export #t)

(import
  :gerbil/gambit/exceptions :gerbil/gambit/os :gerbil/gambit/ports
  :std/misc/list :std/pregexp :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/list)

(def (subpath top . sub-components)
  (path-expand (string-join sub-components "/") top))

(def (absolute-path? path)
  (string-prefix? "/" path))

;; TODO: distinguish between properties of paths and properties of files denoted by those paths.
;; So path-is-absolute? vs path-denotes-symlink?

(def (path-is-symlink? path)
  (eq? 'symbolic-link (file-info-type (file-info path #f))))

(def (path-is-not-symlink? path)
  (not (path-is-symlink? path)))

(def (path-is-file? path (follow-symlinks? #f))
  (eq? 'regular (file-info-type (file-info path follow-symlinks?))))

(def (path-is-directory? path (follow-symlinks? #f))
  (eq? 'directory (file-info-type (file-info path follow-symlinks?))))

;; Given a path to a file that may or may exists on the current filesystem,
;; return a simplified path, eliminating redundant uses of "." or "/",
;; and, unless keep..? is true, also remove ".."
;; (assuming no weird symlinks or mounts that makes you want not to simplify foo/..)
;; NB: Always simplify away a trailing / except for the root directory /.
(def (simplify-path path keep..?: (keep..? #f))
  (def l (string-split path #\/))
  (def abs? (and (pair? l) (equal? (car l) "")))
  (set! l (remove (cut member <> '("" ".")) l))
  (unless keep..?
    (let loop ((head (reverse l)) (tail '()))
      (cond
       ((and (pair? head) (pair? tail) (equal? (car tail) "..") (not (equal? (car head) "..")))
        (loop (cdr head) (cdr tail)))
       ((pair? head)
        (loop (cdr head) (cons (car head) tail)))
       (else (set! l tail))))
    (when abs?
      (while (and (pair? l) (equal? (car l) ".."))
        (set! l (cdr l)))))
  (if (null? l)
    (if abs? "/" "") ;; "" is the standard "here" path, though we could have picked ".".
    (begin
      (when abs?
        (set! l (cons "" l)))
      (string-join l "/"))))

;; Given a path to a file that exists on the filesystem, return
;; a normalized absolute or relative path to it, whichever is shortest
(def (shorten-path x) (path-normalize x 'shortest))

;; If `maybe-subpath` is a pathname that is under `base-path`, return a pathname object that
;; when used with `path-expand` with defaults `base-path`, returns `maybe-subpath`.
(def (subpath? maybe-subpath base-path)
  (and (string? maybe-subpath) (string? base-path)
       (absolute-path? maybe-subpath) (absolute-path? base-path)
       (let ((ls (string-length maybe-subpath))
             (lb (string-length base-path))
             (sep? (λ (s pos) (eqv? (string-ref s pos) #\/))))
         (cond
          ((< ls lb) #f) ;; NB: this in particular concludes that /foo is not subpath of /foo/ ?
          ((> ls lb) (and (or (sep? base-path (- lb 1)) (sep? maybe-subpath lb))
                          (string-prefix? base-path maybe-subpath)
                          (let ((pos (string-index maybe-subpath (λ (x) (not (eqv? x #\/))) lb)))
                            (if pos (substring maybe-subpath pos ls) ""))))
          (else (and (equal? base-path maybe-subpath) ""))))))

;; If `sub` is a pathname that is under `base`, return a pathname string that
;; when used with `path-expand` with defaults `base`, returns `sub`.
(def (enough-path sub base)
  (or (and base (subpath? sub base)) sub))

;; Given a path, visit the path.
;; When the path is a directory and recurse? returns true when called with the path,
;; recurse on the files under the directory.
;; To collect files, poke a list-builder in the visit function.
(def (walk-filesystem-tree!
      path
      visit
      recurse?: (recurse? true)
      follow-symlinks?: (follow-symlinks? #f))

  (def (walk path)
    (visit path)
    (when (and (ignore-errors (path-is-directory? path follow-symlinks?))
               (recurse? path))
      (for-each!
       (directory-files path)
       (λ (name) (walk (path-expand name path))))))

  (walk path))

;; find-files: traverse the filesystem and collect files that satisfy some predicates
;; path: a string that indicates the start point of the recursive filesystem traversal
;; pred?: a predicate that given a path returns true if the path shall be collected
;; recurse?: a function that gigven a path returns true if the traversal shall recurse
;; follow-symlinks?: a boolean that is true if the traversal shall recurse into symlinks
(def (find-files path
                 (pred? true)
                 recurse?: (recurse? true)
                 follow-symlinks?: (follow-symlinks? #f))
  (with-list-builder (collect!)
    (walk-filesystem-tree! path
     (λ (file) (when (pred? file) (collect! file)))
     recurse?: recurse?
     follow-symlinks?: follow-symlinks?)))

(def (total-file-size list-of-files)
  (reduce + 0 (map file-size list-of-files)))

(def (find-regexp-files regexp args)
  (with-list-builder (collect!)
    (for-each!
     args
     (λ (arg)
       (walk-filesystem-tree!
        arg
        (λ (path) (when (and (path-is-file? path)
                             (pregexp-match regexp path))
                    (collect! path))))))))
