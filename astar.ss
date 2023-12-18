(export #t)
(import
  (only-in :std/assert assert!)
  (only-in :std/iter for)
  (only-in :std/misc/pqueue make-pqueue pqueue-empty? pqueue-peek pqueue-pop! pqueue-push!)
  (only-in :std/srfi/1 append-reverse)
  (only-in :std/sugar hash))

(def (A* starts: starts ends: ends +arcs: +arcs -arcs: -arcs) ;; assume cost is real-valued
  (def +q (make-pqueue car)) ;; queue of forward nodes
  (def -q (make-pqueue car)) ;; queue of backward nodes
  (for-each (cut pqueue-push! +q <>) starts)
  (for-each (cut pqueue-push! -q <>) ends)
  (def +t (hash)) ;; table of forward states with best cost
  (def -t (hash)) ;; table of backward nodes with best cost
  (def best [+inf.0 . #f]) ;; cost to beat (once complete paths have been made)
  (def mostcost 0) ;; most cost for an arc
  ;; We could be more precise by instead remembering the least cost
  ;; to a state *preceding* an active path, but that would require more bookkeeping.
  (def (search queue table -table arcs forward?)
    (with ([cost state . path] (pqueue-pop! queue))
      (cond
       ((hash-get table state) ;; previous path to that state
        => (lambda (prev) (assert! (<= (car prev) cost)))) ;; best-path first, so previous must be optimal
       ((hash-get -table state) ;; found a connection!
        => (match <> ([c . p]
                      (let (total (+ cost c))
                        (when (< total (car best))
                          ;;(DBG FOUND!: 'c cost '-c c 'p path '-p p forward?)
                          (let (path (append-reverse path p)) ;; NB: state already in p
                            (set! best [total :: (if forward? path (reverse path))])))))))
       (else
        (hash-put! table state [cost state . path])
        (for (([co . st] (arcs state)))
          (when (> co mostcost) (set! mostcost co))
          (pqueue-push! queue [(+ cost co) st state . path]))))))
  (let loop ()
    (if (or (pqueue-empty? +q) (pqueue-empty? -q))
      best
      (let ((+best (car (pqueue-peek +q)))
            (-best (car (pqueue-peek -q))))
        (if (<= (car best) (- (+ +best -best) mostcost))
          best
          (begin
            (if (<= +best -best)
              (search +q +t -t +arcs #t)
              (search -q -t +t -arcs #f))
            (loop)))))))
