;; Manipulate strings that denoting POSIX-style paths, independently from any underlying filesystem.

(export #t)

(import
  :std/misc/list :std/pregexp :std/srfi/1 :std/srfi/13 :std/sugar
  ./base ./list)

(def (subpath top . sub-components)
  (path-expand (string-join sub-components "/") top))

(def (path-absolute? path)
  (string-prefix? "/" path))

;; Given a path to a file that may or may exists on the current filesystem,
;; return a simplified path, eliminating redundant uses of "." or "/",
;; and, unless keep..? is true, also remove ".."
;; (assuming no weird symlinks or mounts that makes you want not to simplify foo/..)
;; NB: Always simplify away a trailing / except for the root directory /.
(def (path-simplify path keep..?: (keep..? #f))
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

;; If `maybe-subpath` is a pathname that is under `base-path`, return a pathname object that
;; when used with `path-expand` with defaults `base-path`, yields `maybe-subpath`.
;; Otherwise, return #f.
(def (subpath? maybe-subpath base-path)
  (and (string? maybe-subpath) (string? base-path)
       (eq? (path-absolute? maybe-subpath) (path-absolute? base-path))
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
;; Compare CL:ENOUGH-NAMESTRING, UIOP:ENOUGH-PATHNAME.
(def (path-enough sub base)
  (or (and base (subpath? sub base)) sub))

(def (path-normalized-directory path)
  (path-normalize (path-directory path)))

(def (path-parent path)
  (path-simplify (path-expand ".." path)))
