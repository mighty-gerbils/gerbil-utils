;; Persisting Data
(export #t)
(import
  :clan/poo/poo :clan/poo/brace :clan/poo/mop :clan/poo/io
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
(def (<-digest type digest)
  (<-bytes type (db-get (content-addressed-storage-key digest))))

(def (make-dependencies-persistent type x)
  (walk-dependencies type make-persistent x))

(def (make-persistent type x)
  (def k (content-addressed-storage-key (digest type x)))
  (unless (db-key? k)
    (make-dependencies-persistent type x)
    (db-put! k (bytes<- type x))))

;; NB: persistent activities may use dynamic parameters for context.
(.def (PersistentActivity @ Type.
                            repr ;; Any ;; representation for this type of activity
                            key-prefix ;; : u8vector ;; prefix for keys in database
                            Key ;; : Type ;; keys
                            State ;; : Type ;; states
                            ;; the class must provide a way to re-create the activity from
                            ;; the key, a function to persist the state, and the state
                            make-activity) ;; : PersistentActivity <- Key (Unit <- State DbTransaction) State
  loaded: (make-hash-table weak-keys: #t)
  db-key: (lambda (key) (u8vector-append key-prefix (bytes<- Key key)))
  saving: (lambda (db-key state tx)
            (make-dependencies-persistent State state)
            (db-put! db-key (bytes<- State state) tx))
  resume: (lambda (key state) ;; Activity <- Key State
            (when (hash-key? loaded key)
              (error "persistent activity already resumed" repr key))
            (def activity (make-activity key (cut saving (db-key key) <> <>) state))
            (hash-put! loaded key activity)
            activity)
  make-default-state: (lambda (key) ;; override this method to provide a default state
                        (error "Failed to load key %s" repr key))
  make: (lambda (key init)
          (def db-key (db-key key))
          (when (db-key? db-key)
            (error "persistent activity already created" repr key))
          (def state (init (cut saving db-key <> <>)))
          (resume key state))
  get: (lambda (key)
         (or (hash-get loaded key)
             (let (state
                   (cond
                    ((db-get (db-key key)) => (cut bytes<- State <>))
                    (else (make-default-state key))))
               (resume key state)))))
