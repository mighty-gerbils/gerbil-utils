;; Persisting Data
(export #t)
(import
  :gerbil/gambit/threads
  :std/misc/completion :std/misc/queue :std/sugar
  :clan/utils/base :clan/utils/concurrency
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

;; NB: persistent activities may use dynamic parameters for context,
;; such as current database connection, etc.
(.def (PersistentActivity @ Type.
       ;; S-expression that that can be eval'ed back to an equivalent activity
       sexp ;; : Any
       ;; Prefix for keys in database. In a relational DB, that would be the name of the table.
       key-prefix ;; : u8vector
       ;; Type descriptor for keys (to be serialized as DB key)
       Key ;; : Type
       ;; Type descriptor for the persistent state
       State ;; : Type ;; states
       ;; Internal method to re-create the activity from
       ;; (1) the key,
       ;; (2) a function to persist the state,
       ;; (3) the initial state (whether a default state or one read from the database),
       ;; (4) a current transaction context in which the initial state was read.
       ;; The type must provide this method, but users won't use it directly:
       ;; they will call the get method, that will indirectly call make-activity with proper arguments.
       ;; : @ <- Key (Unit <- State TX) State TX
       make-activity)

  ;; Internal table of activities that have already been loaded from database.
  ;; : (Table @ <- Key)
  loaded: (make-hash-table weak-keys: #t weak-values: #t)

  ;; Internal function that associates a key in the key-value store to a user-level key object of type Key.
  ;; : Bytes <- Key
  db-key: (lambda (key) (u8vector-append key-prefix (bytes<- Key key)))

  ;; Internal function that given (1) a db-key (as returned by the function above),
  ;; (2) a current state of type State, and a (3) current transaction context,
  ;; will save said current state in the current transaction.
  ;; Note that this modification will only be committed with the transaction, and
  ;; the activity will have to either synchronously commit-transaction if it owns the transaction,
  ;; or asynchronously call sync-transaction if it doesn't,
  ;; before it may assume the state being committed,
  ;; and start sending according messages to external systems and notifying the user it's done
  ;; (though if transactions have high latency, it might optimistically notify the user
  ;; that the change is underway).
  ;; : <- Bytes State TX
  saving: (lambda (db-key state tx)
            (make-dependencies-persistent State state tx)
            (db-put! db-key (bytes<- State state) tx))

  ;; Internal function that given (1) a db-key (as returned by the function above),
  ;; (2) a current state of type State, and (3) a current transaction context,
  ;; will save said current state in the current transaction.
  ;; Note that this will only be committed with the transaction, and the activity will have to
  ;; either synchronously commit-transaction if it owns the transaction, or asynchronously call
  ;; sync-transaction if it doesn't, before it may assume the state being committed.
  ;; : @ <- Key State TX
  resume: (lambda (key state tx) ;; @ <- Key State
            (when (hash-key? loaded key)
              (error "persistent activity already resumed" sexp key))
            (def activity (make-activity key (cut saving (db-key key) <> <>) state tx))
            (hash-put! loaded key activity)
            activity)

  ;; Internal function that given a key returns a default state to associate with that key
  ;; when no state is found in the database.
  ;; Not all activities have a default state, and the default method will just raise an error.
  ;; : State <- Key
  make-default-state: (lambda (key) ;; override this method to provide a default state
                        (error "Failed to load key" sexp key))

  ;; Function to create a new activity (1) associated to the given key,
  ;; (2) the state of which will be computed by the given initialization function
  ;; (that takes the saving function as argument), (3) in the context of the given transaction.
  ;; Note that any modification will only be committed with the transaction, and
  ;; the init function does not own the transaction and thus will have to call sync-transaction
  ;; or wait for some message by someone that does before it may assume the initial state is committed,
  ;; and start sending according messages to external systems. Similarly, the creating context
  ;; must eventually commit, but any part of it that wants to message based on the activity
  ;; has to sync-transaction to wait for it being saved.
  ;; Also, proper mutual exclusion must be used to ensure only one piece of code
  ;; may attempt create to create an activity with the given key at any point in time.
  ;; : @ <- Key (State <- (<- State TX) TX) TX
  make: (lambda (key init tx)
          (def db-key (db-key key))
          (when (db-key? db-key)
            (error "persistent activity already created" sexp key))
          (def state (init (cut saving db-key <> <>) tx))
          (resume key state tx))

  ;; Function to access an existing activity by (1) its key, and (2) a transaction context.
  ;; For those kinds of activities where it makes sense, this may create a default activity.
  ;; Clients of this code must use proper mutual exclusion so there are no concurrent calls to get.
  ;; Get may indirectly call resume if the object is in the database, and make-default-state if not.
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

  make-activity: ;; Provide the interface function declared above.
  (lambda (key save! state tx)
    (def name [sexp (sexp<- Key key)])
    (spawn/name
     [sexp key]
     (fun (make-persistent-actor)
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

  ;; Run an asynchronous action (1) on the actor with given key, as given by
  ;; (2) a function that takes the current state as a parameter as well as
  ;; a function that sets the state to a new value, in (3) a given transaction context.
  ;; The function will be run asynchronously in the context of the actor,
  ;; and its result will be discarded. To return a value to the caller,
  ;; you must explicitly use a completion, or use the action method below, that does.
  ;; After all actions with a given tx are run, the sync method must be called.
  ;; If the tx is #f then a new transaction will be created in the actor's context
  ;; and synchronized; the action function may then use after-commit to send notifications.
  ;; : Unit <- Key (Unit <- State (<- State)) TX
  async-action:
  (lambda (key k tx)
    (thread-send (get key tx) [Transform: tx k]))

  ;; Run a synchronous action (1) on the actor with given key, as given by
  ;; (2) a function that takes the current state as a parameter as well as
  ;; a function that sets the state to a new value, in (3) a given transaction context.
  ;; The function will be run asynchronously in the context of the actor,
  ;; while the caller waits synchronously for its result as transmitted via a completion.
  ;; After all actions with a given tx are run, the sync method must be called.
  ;; : 'a <- Key ('a <- State (<- State)) TX
  action:
  (lambda (key f tx)
    (def c (make-completion))
    (def (k state set-state!) (completion-post! c (f state set-state!)))
    (async-action key k tx)
    (completion-wait! c))

  ;; Asynchronously notify (1) the actor with the given key that (2) work with the current tx is done;
  ;; the actor will must synchronize with that tx being committed before it starts processing requests
  ;; for other txs.
  ;; : Unit <- Key TX
  sync:
  (lambda (key tx)
    (thread-send (get key tx) [Sync: tx])))
