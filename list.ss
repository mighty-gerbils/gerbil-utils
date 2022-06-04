;; -*- Gerbil -*-
;;;; List utilities
;; TODO: upstream utilities that can be upstreamed to std/misc/list. Keep the rest here.

(export #t)

(import
  :std/misc/list :std/misc/list-builder :std/srfi/1 :std/sugar
  ./base ./hash)

(def alist<-plist plist->alist)
(def plist<-alist alist->plist)

;; Variant of map with arguments reversed, which nest-s nicer.
;; : (List Y) <- (List X) (Y <- X)
(def (list-map list fun)
  (map fun list))

;; : (List (List X)) <- Nat (List X)
(def (group-by n list)
  (cond
   ((null? list) [])
   ((length<=n? list n) [list])
   (else (let-values (((head tail) (split-at list n))) (cons head (group-by n tail))))))

;; Given a list l of X, a key function f from X to Y, and a presumably empty table of Y to a list of X
;; (by default an empty hash-table), add the elements in l to t, then return the list for each added
;; key y in t of the list of elements xs with that same key y (for the equality predicate of t).
;; Otherwise preserve the order of appearance of keys and elements for each key.
;; : (List (List X)) <- (List X) (Fun Y <- X) ? (Table (List X) <- Y)
(def (grouping l f (t (make-hash-table)))
  (def ys (with-list-builder (c)
            (for-each! l
              (lambda (x)
                (def y (f x))
                (def p (hash-get t y))
                (if p
                  (hash-put! t y (cons x p))
                  (begin
                    (hash-put! t y (list x))
                    (c y)))))))
  (map (lambda (y) (reverse (hash-get t y))) ys))

;; : (Cons C B) <- (C <- A) (Cons A B)
(def (map/car f x) (match x ([a . b] [(f a) . b])))

;; Given a predicate, a list and a value to return in the special case that the list is empty,
;; return the special case if the list is empty, otherwise, the smallest element in the list,
;; where the predicate returns true when its first argument is smaller than its second argument.
;; : X <- (Bool <- X X) (List X) X
(def (extremum<-list pred lst (empty-case #f))
  (match lst
    ([] empty-case)
    ([hd . tl]
     (foldl (λ (x y) (if (pred x y) x y)) hd tl))))

;; Given an element of a monoid and a fold function for the monoid,
;; extract a list of the elements in the monoid.
;; : (List A) <- (Monoid A) (B <- (Monoid A) B (B <- A B))
(def (list<-monoid m fold) (fold m '() cons))

;; : (List X) <- (Cons X X)
(def list<-cons (λ-match ([x . y] [x y])))

;; : [Listof Any] Any -> (Or Nat #f)
(def (index-of lst e)
  (list-index (cut equal? e <>) lst))

;; : Bool <- (List X)
(def (not-null? l) (not (null? l)))

;; : (List (NonEmptyList X)) <- (List (List X))
(def (remove-nulls l) (filter not-null? l))

;;; Below is the C3 Linearization algorithm to topologically sort an inheritance DAG
;;; into a precedence list such that direct supers are all included before indirect supers.
;;; It has since been adopted for multiple inheritance by many modern object systems:
;;; Dylan, Python, Raku, Parrot, Solidity, PGF/TikZ.
;;;       https://en.wikipedia.org/wiki/C3_linearization
;;;       https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.19.3910

;; : ((NonEmptyList X) <- X) <- ((List X) <- X)
(def (get-precedence-list<-get-supers get-supers)
  (def (gpl poo)
    (c3-compute-precedence-list
     poo get-supers: get-supers get-precedence-list: gpl))
  gpl)

;; : (List (NonEmptyList X)) <- X (List (NonEmptyList X))
(def (remove-next next tails)
  (remove-nulls (map (lambda (l) (if (equal? (car l) next) (cdr l) l)) tails)))

;; get-name is purely for debugging in case of inconsistent graph
;; : (NonEmptyList A) <- A get-supers:((List A) <- A) \
;;     get-name:?(?<-A) get-precedence-list:?((NonEmptyList A)<-A)
(def (c3-compute-precedence-list
      x get-supers: get-supers get-name: (get-name identity)
      get-precedence-list: (get-precedence-list (get-precedence-list<-get-supers get-supers)))
  (def supers (get-supers x)) ;; : (List A)
  (def super-precedence-lists (map get-precedence-list supers)) ;; : (List (NonEmptyList A))
  (def (c3-select-next tails) ;; : X <- (NonEmptyList (NonEmptyList X))
    (def (candidate? c) (every (lambda (tail) (not (member c (cdr tail)))) tails)) ;; : Bool <- X
    (let loop ((ts tails))
      (when (null? ts) (error "Inconsistent precedence graph" (get-name x)))
      (def c (caar ts))
      (if (candidate? c) c (loop (cdr ts)))))
  (let loop ((rhead [x]) ;; : (NonEmptyList X)
             (tails (remove-nulls (append super-precedence-lists [supers])))) ;; : (List (NonEmptyList X))
    (cond ((null? tails) (reverse rhead))
          ((null? (cdr tails)) (append-reverse rhead (car tails)))
          (else (let (next (c3-select-next tails))
                  (loop (cons next rhead) (remove-next next tails)))))))

;; remove-duplicates with a O(n) algorithm
;; TODO: support start and end keyword arguments? support vectors as well as lists? Meh.
(def (remove-duplicates l test: (test equal?) key: (key identity) from-end?: (from-end? #f))
  (def h (hash-table<-test test))
  (def (run c l)
    (for-each (lambda (x) (def k (key x)) (unless (hash-key? h k) (hash-put! h k #t) (c x))) l))
  (if from-end?
    (with-list-builder (c) (run c l))
    (let (r '()) (run (cut push! <> r) (reverse l)) r)))

(def (pair-tree-for-each! x f)
  (let loop ((x x))
    (match x
      ([a . b] (loop a) (loop b))
      ([] (void))
      (_ (f x)))))

(def (flatten-pair-tree x)
  (with-list-builder (c) (pair-tree-for-each! x c)))

(defrules pushnew! ()
  ((pushnew! element list) (pushnew! element list equal?))
  ((pushnew! element list test) (let (x element) (unless (member x list test) (push! x list)))))

(def (alist<-fun-list f l)
  (map (lambda (x) (cons x (f x))) l))

(def (alist<-hash-list h l)
  (alist<-fun-list (cut hash-get h <>) l))

;; Similar to with-list-builder, but proactively removes duplicates,
;; as per the specified equality predicate or hash-table (that specifies the predicate),
;; wherein the equality predicate is one of equal? eqv? eq?, where () signifies equal?
(defrules with-deduplicated-list-builder (equal? eqv? eq?)
  ((recur equal (poke) body1 body+ ...)
   (recur equal (poke _unused) body1 body+ ...))
  ((_ equal (poke peek) body1 body+ ...)
   (let (h (specify-hash-table equal?))
     (with-list-builder (primpoke peek)
       (defrules poke ()
         ((_ val) (unless (hash-key? h val) (hash-put! h val #t) (primpoke val)))
         (id (identifier? #'id) (lambda (val) (poke val))))
       body1 body+ ...))))

(defrule (call-with-deduplicated-list-builder fun (table (make-hash-table)))
  (with-deduplicated-list-builder table (poke peek) (fun poke peek)))

