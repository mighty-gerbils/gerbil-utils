;; Persisting Data
(export #t)
(import
  :gerbil/gambit/threads
  :std/misc/completion :std/misc/queue :std/sugar
  :clan/utils/concurrency
  :clan/poo/poo :clan/poo/mop :clan/poo/io
  ./db)

(.defgeneric (walk-dependencies type f x) ;; Unit <- 'a:Type (Unit <- 'b:Type 'b) 'a
   slot: .walk-dependencies from: methods default: void)

(.defgeneric (digest type b)
   slot: .digest from: methods)

(def content-addressed-storage-prefix (string->bytes "CA"))

(def (content-addressed-storage-key digest)
  (u8vector-append content-addressed-storage-prefix digest))

;; Load an object from database based on its contents address
;; 'a <- 'a:Type Digest
(def (<-digest type digest tx)
  (<-bytes type (db-get (content-addressed-storage-key digest) tx)))

(def (make-dependencies-persistent type x tx)
  (walk-dependencies type (cut make-persistent <> <> tx) x))

(def (make-persistent type x tx)
  (def k (content-addressed-storage-key (digest type x)))
  (unless (db-key? k tx)
    (make-dependencies-persistent type x tx)
    (db-put! k (bytes<- type x) tx)))

;; NB: persistent activities may use dynamic parameters for context.
(.def (PersistentActivity @ Type.
                            sexp ;; Any ;; representation for this type of activity
                            key-prefix ;; : u8vector ;; prefix for keys in database
                            Key ;; : Type ;; keys
                            State ;; : Type ;; states
                            ;; the class must provide a way to re-create the activity from
                            ;; the key, a function to persist the state, and the state
                            ;; : PersistentActivity <- Key (Unit <- State TX) State TX
                            make-activity)

  ;; : (Table @ <- Key)
  loaded: (make-hash-table weak-keys: #t)

  ;; : Bytes <- Key
  db-key: (lambda (key) (u8vector-append key-prefix (bytes<- Key key)))

  ;; : <- Bytes State TX
  saving: (lambda (db-key state tx)
            (make-dependencies-persistent State state tx)
            (db-put! db-key (bytes<- State state) tx))

  ;; : @ <- Key State TX
  resume: (lambda (key state tx) ;; @ <- Key State
            (when (hash-key? loaded key)
              (error "persistent activity already resumed" sexp key))
            (def activity (make-activity key (cut saving (db-key key) <> <>) state tx))
            (hash-put! loaded key activity)
            activity)

  ;; : State <- Key
  make-default-state: (lambda (key) ;; override this method to provide a default state
                        (error "Failed to load key" sexp key))

  ;; : @ <- Key (State <- (<- State TX) TX) TX
  make: (lambda (key init tx)
          (def db-key (db-key key))
          (when (db-key? db-key)
            (error "persistent activity already created" sexp key))
          (def state (init (cut saving db-key <> <>) tx))
          (resume key state tx))

  ;; : @ <- Key TX
  get: (lambda (key tx)
         (or (hash-get loaded key)
             (let (state
                   (cond
                    ((db-get (db-key key) tx) => (cut <-bytes State <>))
                    (else (make-default-state key))))
               (resume key state tx)))))

(.def (PersistentActor @ PersistentActivity
                         Key sexp get)

  ;; : @ <- Key (Unit <- State DbTransaction) State
  make-activity:
  (lambda (key save! state tx)
    (def name [sexp (sexp<- Key key)])
    (spawn/name
     [sexp key]
     (lambda ()
       (def owner #f)
       (def (check-owner tx)
         (unless (eq? owner tx) (error "bad transaction" [sexp key] owner tx)))
       (def (set-state-default x)
         (check-owner #f) (with-new-tx (tx) (save! state tx) (set! state x)))
       (def set-state! set-state-default)
       (def txq (make-queue)) ;; queue of txs to process
       (def msgs (make-hash-table)) ;; maps tx to reverse sequence of relevant messages.
       (def (process msg)
         (match msg
           ([Read: k] (k state))
           ([Transform: tx k]
            (cond
             ((eq? tx owner)
              (k state set-state!))
             ((not owner)
              (set! owner tx)
              (set! set-state!
                (lambda (x) (check-owner tx) (save! state tx) (set! state x)))
              (k state set-state!))
             (else
              (let (id (DbTransaction-txid tx))
                (hash-put! msgs id (cons msg (or (hash-get msgs id)
                                                 (begin (enqueue! txq id) []))))))))
           ([Sync: tx]
            (check-owner tx)
            (set! owner #f)
            (set! set-state! set-state-default)
            (sync-transaction tx)
            (unless (queue-empty? txq)
              (let* ((id (dequeue! txq))
                     (l (reverse (hash-get msgs id))))
                (hash-remove! msgs id)
                (for-each process l))))))
       (while #t (process (thread-receive))))))

  ;; Run an action on the actor's state
  ;; : 'a <- @ ('a <- State (<- State)) DbTransaction
  action:
  (lambda (actor f tx)
    (def c (make-completion))
    (def (k state set-state!)
      (completion-post! c (f state set-state!)))
    (thread-send actor [Transform: tx k])
    (completion-wait! c)))
