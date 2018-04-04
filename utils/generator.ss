;; -*- Gerbil -*-
;;;; Effectful generators as fonts of data elements
;;;
;;; Generators are data fonts, producing an effectful stream of data elements,
;;; to be somehow connected to a data sink that will consume the elements.
;;; They can be viewed in several complementary ways:
;;; Iterators (as per std/iter) are mutable objects that allow their user to advance in the stream
;;; one element at a time, querying the stream in a pull interface.
;;; Iterating functions, like for-each!, take a function as argument, and sequentially call it
;;; with each element in the stream, in a push interface.
;;; Generating functions, are a pull interface like iterators, packaged in a single procedure,
;;; like python "iterators", that returns the next element every time it's called, until it
;;; reaches the end and raises an #!eof instead.
;;;
;;; The "push" and "pull" family of interfaces are dual views related to each other
;;; by *inversion of control* (i.e. use of call/cc).
;;; Gerbil uses first class continuations and/or actors to implement inversion of control
;;; within the language, in a way more powerful than e.g. the limited system-provided pattern
;;; available within Python to define iterators by specifyingn a generator.
;;; For instance, our "yield" is a regular function can be called in a subroutine
;;; or passed to a higher-order function.
;;;
;;; A generator will generate its values only once.
;;; Using one of the interfaces commits the user to keep using that interface,
;;; and it is not specified what happens if the user uses one then the other.
;;; Specific generators can specify what happens.
;;;
;;; TODO: Also have a concurrent variant, that sends asynchronous messages in parallel,
;;; rather than call synchronous functions in sequence.
;;; Also have a pure functional interface in terms of functors and map.
;;; Extract all the variants from a single specification using a description in linear logic,
;;; in the style of my 2012 article about my Lisp-Interface-Library.
;;;
;;; Still unsatisfactorily using dynamic raise and side-effects rather than a pure interface.
;;; TODO: a monadic alternative? one with pure call/cc but no other side-effects
;;; and static continuations?
;;;
;;; Riastradh proposes to use delimited continuations as in:
;;; (define ((generator f)) (shift f))
;;; (define-syntax for (syntax-rules () ((for x g b0 b1+ ...) (reset (let ((x (g))) b0 b1+ ...)))))
;;; (for x (generator (lambda (yield) (yield 0) (yield 1) (yield 2))) (pp x))
;;; https://mumble.net/~campbell/scheme/shift-reset.scm
;;; https://mumble.net/~campbell/scheme/fshift-freset.scm
;;;
;;; TODO: deprecate in favor of SRFI 158 generators?
;;; https://srfi.schemers.org/srfi-158/srfi-158.html ?
;;; Or make a counter-proposal?
;;; They use an in-band fixed eof object instead of out-of-band exception,
;;; probably because there is no standard exception mechanism in Scheme.
;;; Should eof be a parameter? With a special form to rebind it to a fresh local object?
;;;
;;; TODO: Do generators as actor? A lighter-weight actor syntax would be nice, too.
;;;


(export #t)

(import
  :gerbil/gambit/threads
  :std/actor :std/coroutine :std/iter :std/misc/list :std/misc/pqueue :std/misc/queue :std/sugar
  :clan/utils/base :clan/utils/list :clan/utils/number :clan/utils/vector)


;; A generating function of type A is a function that each time it is called,
;; either generates an A or calls a failure continuation passed to it,
;; which by default raises #!eof as an exception.
;; The function may otherwise have side-effects.
;; type (Generating A) = (A <-* (Optional (<-* (<-* A)) eof!))

;; The empty generating function and default failure continuation for generating functions,
;; that only generates eof.
;; type Eof = (Exception '#!eof)
;; _ ... <-[Eof] _ ...
(def (eof! . _) (while #t (raise #!eof)))

;; A function to call if a yield function was called unexpectedly,
;; e.g. due to undesired continuation capture.
;; _ ... <-[Error] _ ...
(def (unexpected-yield . vals) (error "unexpected yield" vals))

;; The empty generating function, does not generate any element.
;; : (Generating _)
(def (generating-empty (on-eof eof!)) (on-eof))


;; Control inversion: given a generating function that takes a 'yield' function,
;; and call 'yield' it with various values until it eventually returns (if it does),
;; return a generator function that, each time it is called, evaluates a bit of the generating
;; function and returns the next value generated that 'yield' is called with,
;; until the generating function returns, at which point always raise the eof object.
;; Beware: this doesn't play well with try/finally blocks; for these use generating<-coroutine
;; (Generating A) <- (... <-* (<-* A))
(def (generating<-for-each for-each)
  (letrec
      ((yield-k unexpected-yield)
       (eof-k eof!)
       (generating (λ () (for-each yield) (eof-k)))
       (yield (λ vals (let/cc k (set! generating k) (!!> vals yield-k)))))
    (λ ((on-eof eof!))
      (let/cc k (set! yield-k k) (set! eof-k on-eof) (generating)))))

;; (Generating A) <- (List A)
(def (generating<-list list)
  (generating<-for-each (curry for-each! list)))

;; (List A) <- (Generating A)
(def (list<-generating generating)
  (with-list-builder (c!) (generating-for-each! generating c!)))

;; (Generating A) <- (Vector A) Nat (Or Nat '#f)
(def (generating<-vector vector start: (start 0) end: (end #f))
  (generating<-for-each (λ (yield) (vector-for-each! vector yield start: start end: end))))

;; (Generating A) <- (Vector A) Nat (Or Nat '#f)
(def (generating-reverse<-vector vector start: (start 0) end: (end #f))
  (generating<-for-each (λ (yield) (vector-reverse-for-each! vector yield start: start end: end))))

;; (Vector A) <- (Generating A)
(def (vector<-generating generating)
  (list->vector (list<-generating generating)))

;; (Generating A) <- (Generating A)
(def (generating-reverse generating)
  (λ (self) (generating-reverse<-vector (vector<-generating generating))))

;; <- (Generating A) (<- A)
(def (generating-for-each! generating fun)
  (let/cc return
    (while #t (fun (generating return)))))

;; Nat <- (Generating _)
(def (generating-count generating)
  (let ((i 0))
    (generating-for-each! generating (λ (_) (increment! i)))
    i))

;; (Generating B) <- (Generating B) (B <- A)
(def (generating-map generating fun)
  (compose fun generating))

;; Fold a dataset from earliest to latest elements
;; B <- (Generating A) B (B <- A B)
(def (generating-fold generating seed fun)
  (generating-for-each! generating (λ (val) (set! seed (fun val seed))))
  seed)

(def (generating-take generating n (short-ok? #f))
  (with-list-builder (collect!)
    (try
     (for ((_ (in-range 0 n))) (collect! (generating)))
     (catch (λ (c) (and short-ok? (eof-object? c))) => void))))

(def (generating-take-reverse generating n (short-ok? #f))
  (reverse (generating-take generating n short-ok?)))

(def (generating-drop-until generating pred)
  (let ((next (generating)))
    (if (pred next) next (generating-drop-until generating pred))))

(def (make-generating-peeking generating)
  (letrec
      ((eof? #f)
       (peeked? #f)
       (next #f)
       (get
        (λ ()
          (unless eof?
            (set! peeked? #t)
            (set! next (generating (λ () (set! eof? #t) (set! generating #f) #f))))))
       (peek
        (λ ((eof #!eof))
          (cond
           (eof? eof)
           (peeked? next)
           (else (get) (peek eof)))))
       (generate
        (λ ((on-eof eof!))
          (cond
           (eof? (on-eof))
           (peeked? (set! peeked? #f) next)
           (else (get) (generate on-eof))))))
    (values generate peek)))

(def (iter<-generating-peeking generating peeking)
  (def (value-e _) (peeking iter-end))
  (def (next-e _) (generating))
  (make-iterator #f void value-e next-e))

(def (iter<-generating generating)
  (call-with-values (λ () (make-generating-peeking generating)) iter<-generating-peeking))

(def (iter-for-each! iter fun)
  (for (x iter) (fun x)))

(def (generating<-iter iter)
  (generating<-for-each (cut iter-for-each! iter <>)))

(def generating-for-each-until
  (let ((my-eof #(#!eof)))
    (λ (generating peek pred fun (eof #!eof))
      (let ((next (peek my-eof)))
        (cond
         ((eq? next my-eof) eof)
         ((pred next) next)
         (else (generating) (generating-for-each-until generating peek pred fun eof)))))))

(def (generating-peeking-drop-until generating peek pred (eof #!eof))
  (generating-for-each-until generating peek pred void eof))

(def (generating-elements-until generating peek pred)
  (with-list-builder (c!) (generating-for-each-until generating peek pred c!)))

;;;; Control inversion using an actor.
;; Because Gambit doesn't seem to have delimited continuations that play well with finally blocks,
;; you can't use the above generating<-for-each with code that uses them.
;; The solution is to run the routine in another thread, the coroutine.

;; (Generating A) <- (... <- (<- A))
(def (generating<-coroutine generating-function)
  ;; The naive generating<-for-each works great as long as
  ;; the generating function doesn't have any finally clause,
  ;; because these clauses are triggered by the inversion, and then the generation fails.
  ;; To truly fix this issue would require using some lower-level delimited continuation API
  ;; that subverts the finally detection mechanism.
  ;; A workaround is to run the generating function in a generating thread;
  ;; the thread will be started at the first call, and blocked until needed again.
  (def (coroutine-function) (try (generating-function yield) (finally (raise #!eof))))
  (def cort (coroutine coroutine-function))
  (def (generating (on-eof eof!))
    (try (continue cort) (catch (eof-object? _) (on-eof))))
  (def (shutdown) (coroutine-stop! cort))
  (values generating shutdown))

(def (generating-peeking<-coroutine generating-function)
  (defvalues (generating shutdown)
    (generating<-coroutine generating-function))
  (defvalues (next peek)
    (make-generating-peeking generating))
  (values next peek shutdown))

;; Split a generating into two generatings,
;; the generated elements of which respectively satisfy and don't satisfy the given predicate
;; : (Generating A) (Generating A) <- (Generating A) (Bool <- A)
(def (generating-partition generating pred)
  (def yes-queue (make-queue)) ;; or use explicit actors with async message send?
  (def no-queue (make-queue))
  (def (yes-generating (on-eof eof!))
    (let/cc return
      (if (queue-empty? yes-queue)
        (let loop ()
          (let ((val (generating (λ () (return (on-eof))))))
            (if (pred val) (return val) (enqueue! no-queue val))
            (loop)))
        (dequeue! yes-queue))))
  (def (no-generating (on-eof eof!))
    (let/cc return
      (if (queue-empty? no-queue)
        (let loop ()
          (let ((val (generating (λ () (return (on-eof))))))
            (if (pred val) (enqueue! yes-queue val) (return val))
            (loop)))
        (dequeue! no-queue))))
  (values yes-generating no-generating))


;; Merge generators being generated into a single generator:
;; given an (Generating (Generating A)), where As can be totally ordered by priority,
;; and each Generating function generates value by decreasing priority,
;; return an Generator that for every priority level returns
;; a merge of all the As at that level of priority.
;; If you squint, this implements sparse matrix transposition, where
;; you go from having a row of columns to having a column of rows.
;; The function merge defaults to the identity function.
;; The function priority extracts the priority level from a generated value.
;; The function priority= compares priority for equality, and determines whether consecutive
;; entries will be merged.
;; The function priority< compares priority by order.
;; If (priority< p1 p2) then p1 has higher priority
;; (e.g. using <, lower numbers come first, and the numbers will be increasing)
;; : (Generating B) <-
;;     (Generating (Generating A))
;;     merge: (Optional (B <- (Generating A)) identity)
;;     priority: (Optional (P <- A) identity)
;;     priority=: (Optional (Bool <- P P) equal?)
;;     priority<: (Optional (Bool <- P P) <)
(def (generating-merge
      gen-gen
      merge: (merge identity)
      priority: (priority identity)
      priority=: (priority= equal?)
      priority<: (priority< <))
  (def pq (make-pqueue car priority<))
  (def (consider generating)
    (let/cc return
      (let ((value (generating return)))
        (pqueue-push! pq [(priority value) value . generating]))))
  (generating-for-each! gen-gen consider)
  (λ ((on-eof eof!))
    ;; Initialize a pqueue (priority queue) of generators indexed by priority of next value
    ;; The current value is a merge of the values at that priority (in no guaranteed order)
    (nest
     (if (pqueue-empty? pq) (on-eof))
     (merge) (generating<-for-each) (λ (yield))
     (match (pqueue-pop! pq)) ([pri val . gen])
     (let loop ((val val) (gen gen))
       (consider gen)
       (yield val))
     (unless (pqueue-empty? pq))
     (match (pqueue-peek pq)) ([next-pri next-val . next-gen])
     (when (priority= next-pri pri)
       (pqueue-pop! pq)
       (loop next-val next-gen)))))

;; Flatten a generator of generator of A into a single generator of A:
;; : (Generating A) <- (Generating (Generating A))
(def (generating-flatten generating)
  (generating<-for-each
   (λ (yield) (generating-for-each! generating (λ (gen) (generating-for-each! gen yield))))))

(def (generating-singleton x)
  (generating<-for-each (λ (yield) (yield x))))

(def (generating-filter pred g)
  (generating<-for-each (λ (yield) (generating-for-each! g (λ (x) (when (pred x) (yield x)))))))

(def generating-concat
  (case-lambda
    (() generating-empty)
    ((g) g)
    ((g1 g2) (generating<-for-each
              (λ (yield) (generating-for-each! g1 yield) (generating-for-each! g2 yield))))
    ((g1 g2 g3 . gs) (generating-flatten (generating<-list [g1 g2 g3 . gs])))))

(def (generating-peeking-shutting<-shutdown-generating shutdown generating)
  (defvalues (next peek) (make-generating-peeking generating))
  (values next peek shutdown))
