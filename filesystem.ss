;; -*- Gerbil -*-
;;;; Utilities to interface the filesystem

(export #t)

(import
  :gerbil/gambit/exceptions :gerbil/gambit/os :gerbil/gambit/ports
  :std/misc/list :std/pregexp :std/srfi/1 :std/sugar
  ./base ./list ./path)

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

;; Given a path to a file that exists on the filesystem, return
;; a normalized absolute or relative path to it, whichever is shortest
(def (shorten-path x) (path-normalize x 'shortest))

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
