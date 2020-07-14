;; -*- Gerbil -*-
;;; Stateful AVL-tree
;; adapted from Common Lisp's lisp-interface-library

(export #t)

(import
  :std/iter :std/misc/list :std/misc/repr :std/sugar
  ./base ./generator)

;; NB: this field order makes the representation naturally sorted.
(defstruct avl-map
  (left height key value right))

(def (avl-map-empty? node) ;; NB: only the top-level node can be empty; empty children are #f
  (or (not node) (zero? (avl-map-height node))))
(def (make-empty-avl-map)
  (make-avl-map #f 0 #f #f #f))

(def (avl-map-lookup key-comparer node key)
  (if (avl-map-empty? node)
    (values #f #f)
    (case (key-comparer key (avl-map-key node))
      ((0) (values (avl-map-value node) #t))
      ((-1) (avl-map-lookup key-comparer (avl-map-left node) key))
      ((1) (avl-map-lookup key-comparer (avl-map-right node) key)))))

(def avl-map-ref
  (case-lambda
    ((key-comparer node key default)
     (defvalues (value found?) (avl-map-lookup key-comparer node key))
     (if found? value default))
    ((key-comparer node key)
     (defvalues (value found?) (avl-map-lookup key-comparer node key))
     (if found? value (error "Key not found" node key)))))

(def (avl-map-get key-comparer node key)
  (avl-map-ref key-comparer node key #f))

(def (avl-map-key? key-comparer node key)
  (nth-value 1 (avl-map-lookup key-comparer node key)))

(def (avl-map-key-value node) ;; NB: only meaningful if not empty
  (values (avl-map-key node) (avl-map-value node)))

(def (avl-map-leftmost-node node) ;; NB: only meaningful if not empty
  (if-let (left (avl-map-left node))
    (avl-map-leftmost-node left)
    node))

(def (avl-map-leftmost node) ;; NB: only meaningful if not empty
  (avl-map-key-value (avl-map-leftmost-node node)))

(def (avl-map-rightmost-node node) ;; NB: only meaningful if not empty
  (if-let (right (avl-map-right node))
    (avl-map-rightmost-node right)
    node))

(def (avl-map-rightmost node) ;; NB: only meaningful if not empty
  (avl-map-key-value (avl-map-rightmost-node node)))


;;; Methods used for balancing trees
(def (avl-map-get-height node)
  (if node (avl-map-height node) 0))

(def (avl-map-update-height! node)
  (unless (avl-map-empty? node)
    (set! (avl-map-height node)
      (1+ (max (avl-map-get-height (avl-map-left node))
               (avl-map-get-height (avl-map-right node)))))))

(def (avl-map-rotate-right! node)
  ;; (LL2 C:KL LR2) N:K R1 ==> (LL2 C:KL (LR2 N:K R1))
  (let ((child (avl-map-left node)))
    (rotate! (avl-map-key child) (avl-map-key node))
    (rotate! (avl-map-value child) (avl-map-value node))
    (rotate! (avl-map-left node) (avl-map-left child) (avl-map-right child) (avl-map-right node))
    (avl-map-update-height! child)
    (avl-map-update-height! node)))

(def (avl-map-rotate-left! node)
  ;; L1 N:K (RL2 C:KR RR2) ==> (L1 C:K RL2) N:KR RR2
  (let ((child (avl-map-right node)))
    (rotate! (avl-map-key child) (avl-map-key node))
    (rotate! (avl-map-value child) (avl-map-value node))
    (rotate! (avl-map-right node) (avl-map-right child) (avl-map-left child) (avl-map-left node))
    (avl-map-update-height! child)
    (avl-map-update-height! node)))

(def (avl-map-balance node)
  (- (avl-map-get-height (avl-map-right node))
     (avl-map-get-height (avl-map-left node))))

(def (avl-map-balance! node)
  (case (avl-map-balance node)
    ((-1 0 1) ;; already balanced, just update height
     (avl-map-update-height! node))
    ((-2)
     (case (avl-map-balance (avl-map-left node))
       ;;((-1 0) (void))
       ((1)
        (avl-map-rotate-left! (avl-map-left node))))
     (avl-map-rotate-right! node))
    ((2)
     (case (avl-map-balance (avl-map-right node))
       ;;((0 1) (void))
       ((-1)
        (avl-map-rotate-right! (avl-map-right node))))
     (avl-map-rotate-left! node))))

(def (avl-map-ensure-left node)
  (or (avl-map-left node)
      (let ((new (make-empty-avl-map)))
        (set! (avl-map-left node) new)
        new)))

(def (avl-map-ensure-right node)
  (or (avl-map-right node)
      (let ((new (make-empty-avl-map)))
        (set! (avl-map-right node) new)
        new)))

(def (avl-map-put! key-comparer node key value)
  (if (avl-map-empty? node)
    (begin
      (set! (avl-map-key node) key)
      (set! (avl-map-value node) value)
      (set! (avl-map-height node) 1))
    (case (key-comparer key (avl-map-key node))
      ((0) (set! (avl-map-value node) value))
      ((-1) (avl-map-put! key-comparer (avl-map-ensure-left node) key value))
      ((1) (avl-map-put! key-comparer (avl-map-ensure-right node) key value))))
  (avl-map-balance! node))


(def (avl-map-copy-node! destination origin)
  (set! (avl-map-left destination) (avl-map-left origin))
  (set! (avl-map-height destination) (avl-map-height origin))
  (set! (avl-map-key destination) (avl-map-key origin))
  (set! (avl-map-value destination) (avl-map-value origin))
  (set! (avl-map-right destination) (avl-map-right origin)))

(def (avl-map-remove! key-comparer node key (default #f))
  (if (avl-map-empty? node)
    default
    (begin0
        (case (key-comparer key (avl-map-key node))
          ((0)
           (begin0
               (avl-map-value node)
             (cond
              ((and (avl-map-left node) (avl-map-right node))
               ;; TODO: have a way to pick (based on balance information?)
               ;; which of "leftmost of the right side"
               ;; and "rightmost of the left side" to pick
               (let-values (((kk vv) (avl-map-leftmost (avl-map-right node))))
                 (avl-map-remove! key-comparer (avl-map-right node) kk)
                 (when (avl-map-empty? (avl-map-right node))
                   (set! (avl-map-right node) #f))
                 (set! (avl-map-key node) kk)
                 (set! (avl-map-value node) vv)))
              ((avl-map-left node) => (λ (left) (avl-map-copy-node! node left)))
              ((avl-map-right node) => (λ (right) (avl-map-copy-node! node right)))
              (else ;; empty!
               (set! (avl-map-key node) #f)
               (set! (avl-map-value node) #f)
               (set! (avl-map-height node) 0)))))
          ((-1)
           (begin0
               (avl-map-remove! key-comparer (avl-map-left node) key)
             (when (avl-map-empty? (avl-map-left node))
               (set! (avl-map-left node) #f))))
          ((1)
           (begin0
               (avl-map-remove! key-comparer (avl-map-right node) key)
             (when (avl-map-empty? (avl-map-right node))
               (set! (avl-map-right node) #f)))))
      (avl-map-balance! node))))

(def (avl-map-for-each! map fun)
  (unless (avl-map-empty? map)
    (avl-map-for-each! (avl-map-left map) fun)
    (fun (cons (avl-map-key map) (avl-map-value map)))
    (avl-map-for-each! (avl-map-right map) fun)))

(def (generating<-avl-map map)
  (generating<-for-each (cut avl-map-for-each! map <>)))

(def (avl-map-size map)
  (generating-count (generating<-avl-map map)))



(def (avl-map-for-each-in-reverse! map fun)
  (unless (avl-map-empty? map)
    (avl-map-for-each-in-reverse! (avl-map-right map) fun)
    (fun (cons (avl-map-key map) (avl-map-value map)))
    (avl-map-for-each-in-reverse! (avl-map-left map) fun)))

(def (generating-reverse<-avl-map map)
  (generating<-for-each (cut avl-map-for-each-in-reverse! map <>)))

(def (avl-map-take-left map n (short-ok? #f))
  (generating-take (generating<-avl-map map) n short-ok?))

(def (avl-map-take-right map n (short-ok? #f))
  (generating-take-reverse (generating-reverse<-avl-map map) n short-ok?))

(def (avl-map<-table key-comparer table)
  (let ((m (make-empty-avl-map)))
    (hash-for-each (λ (k v) (avl-map-put! key-comparer m k v)) table)
    m))

(def (table<-avl-map map)
  (let ((h (make-hash-table)))
    (avl-map-for-each! map (λ-match ([k . v] (hash-put! h k v))))
    h))

(def (avl-map<-alist key-comparer table)
  (let ((m (make-empty-avl-map)))
    ;; NB: in an alist, only the first mapping counts
    (nest
     (for-each! table) (λ-match) ([k . v])
     (unless (avl-map-key? key-comparer m k))
     (avl-map-put! key-comparer m k v))
    m))

(def (alist<-avl-map map)
  (with-list-builder (c!) (avl-map-for-each! map c!)))

(def (avl-map-singleton key value)
  (make-avl-map #f 0 key value #f))

;; Print low-level representation.
(defmethod {:pr avl-map}
  (λ (m (port (current-output-port)) (options (current-representation-options)))
    (def (p y) (pr y port options))
    (def (d y) (display y port))
    (d "(make-avl-map ")
    (p (avl-map-left m)) (d " ")
    (p (avl-map-height m)) (d " ")
    (p (avl-map-key m)) (d " ")
    (p (avl-map-value m)) (d " ")
    (p (avl-map-right m)) (d ")")))

(defmethod {:json avl-map}
  (λ (m)
    (map (λ-match ([k . v] (vector k v))) (alist<-avl-map m))))

(def (call-with-avl-map fun (initial-alist '()) (comparer number-comparer))
  (let ((m (avl-map<-alist comparer initial-alist)))
    (fun m)
    m))

(defrules with-avl-map ()
  ((_ (m init ...) body ...) (call-with-avl-map (λ (m) body ...) init ...)))
