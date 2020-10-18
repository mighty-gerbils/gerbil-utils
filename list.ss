;; -*- Gerbil -*-
;;;; List utilities
;; TODO: upstream utilities that can be upstreamed to std/misc/list. Keep the rest here.

(export #t)

(import
  :std/misc/list :std/sugar :std/srfi/1
  ./base)

(def alist<-plist plist->alist)
(def plist<-alist alist->plist)

;; Variant of map with arguments reversed, which nest-s nicer.
;; : (list Y) <- (list X) (Y <- X)
(def (list-map list fun)
  (map fun list))

(def (group-by n list)
  (cond
   ((null? list) [])
   ((length<=n? list n) [list])
   (else (let-values (((head tail) (split-at list n))) (cons head (group-by n tail))))))

(def (map/car f x) (match x ([a . b] [(f a) . b])))

;; Given a predicate, a list and a value to return in the special case that the list is empty,
;; return the special case if the list is empty, otherwise, the smallest element in the list,
;; where the predicate returns true when its first argument is smaller than its second argument.
;; : X <- (Bool <- X X) (list X) X
(def (extremum<-list pred lst (empty-case #f))
  (match lst
    ([] empty-case)
    ([hd . tl]
     (foldl (λ (x y) (if (pred x y) x y)) hd tl))))

;; Given an element of a monoid and a fold function for the monoid,
;; extract a list of the elements in the monoid.
;; (List A) <- (Monoid A) (B <- (Monoid A) B (B <- A B))
(def (list<-monoid m fold) (fold m '() cons))

(def list<-cons (λ-match ([x . y] [x y])))

;; index-of : [Listof Any] Any -> (Or Nat #f)
(def (index-of lst e)
  (list-index (cut equal? e <>) lst))
