;; -*- Gerbil -*-
;;;; Basic helpers for JSON

(export #t)

(import
  :gerbil/gambit/ports
  :std/iter :std/misc/alist :std/misc/hash :std/misc/list-builder
  :std/misc/ports :std/misc/plist :std/misc/rtd
  :std/sort :std/srfi/43 :std/sugar :std/text/json
  ./base ./basic-parsers ./list ./files ./subprocess)

(def (trivial-json<-object object)
  (match (class->list object)
    ([type . plist]
     (list->hash-table
      `(#|(__class . ,(symbol->string (type-name type)))|# ,@(plist->alist plist))))))
(def (trivial-object<-json klass json)
  (def (find-key s) (or (##find-interned-keyword s) (error "invalid json key for class" s klass)))
  (apply make-class-instance klass (alist->plist (map (cut map/car find-key <>) (hash->list json)))))

(def (trivial-json<-struct struct)
  (defvalues (strukt fields) (cons->values (struct->list struct)))
  (def names (cdr (assoc fields: (type-descriptor-plist strukt))))
  (def json (make-hash-table))
  (def f (if (json-symbolic-keys) identity symbol->string))
  (for ((name names) (v fields)) (hash-put! json (f name) v))
  json)

(def (trivial-struct<-json strukt json (defaults #f))
  (unless defaults (set! defaults (hash)))
  (def names (list->vector (cdr (assoc fields: (type-descriptor-plist strukt)))))
  (def positions (invert-hash<-vector names))
  (def (pos<-field f)
    (def s (cond
            ((symbol? f) f)
            ((string? f) (##find-interned-symbol f))
            (else #f)))
    (or (hash-get positions s)
        (error "invalid json key for struct" f strukt json)))
  (def n (vector-length names))
  (def fields (make-vector n #f))
  (def bound? (make-vector n #f))
  (for (((values k v) (in-hash json)))
    (let (p (pos<-field k))
      (when (vector-ref bound? p) (error "field multiply defined" k strukt json))
      (vector-set! bound? p #t)
      (vector-set! fields p v)))
  (def unbounds
    (with-list-builder (c)
     (for ((i (in-naturals))
           (b? bound?)
           (name names))
       (cond
        (b? (void))
        ((hash-key? defaults name) (vector-set! fields i (hash-ref defaults name)))
        (else (c name))))))
  (unless (null? unbounds)
    (error "unbound fields" unbounds strukt json))
  (apply make-struct-instance strukt (vector->list fields)))

;; Mixin for a trivial method that just lists all slots
(defclass jsonable ())
(defmethod {:json jsonable} trivial-json<-object)

(def (pretty-json object)
  (filter-with-process
   ["jq" "-M" "."]
   (λ (port) (write-json object port))
   read-all-as-string))

(def (pretty-print-json object (port (current-output-port)))
  (display (pretty-json object) port)
  (newline port))

(def (json<-string x)
  (call-with-input-string
   x (lambda (port)
       (begin0 (json<-port port)
         (expect-and-skip-any-whitespace port)
         (expect-eof port)))))

(def (string<-json object)
  (parameterize ((json-symbolic-keys #f))
    (json-object->string object)))

;; TODO: rename to safe-read-json or read-json/string-keys or something
(def (json<-port port)
  (parameterize ((json-symbolic-keys #f)) ;; Don't intern JSON keys
    (read-json port)))

;; For better performance when skipping, parse json lazily.
(def (lazy-json<-string string (decode identity))
  (delay
    (decode
     (begin0 (json<-string string)
       (set! string #f))))) ;; reclaim storage (or will gambit do it based on lifespan analysis?)

(def (expect-lazy-json-line port (decode identity))
  (lazy-json<-string (read-line port)))

(def (read-file-json file . settings)
  (call-with-input-file (cons* path: file settings) json<-port))

(def (write-file-json file json . settings)
  (clobber-file file (curry write-json json) settings: settings))

(def (json-normalize x)
  (json<-string (string<-json x)))

(def (write-json-ln x (port (current-output-port)))
  (write-json x port) (newline port))

(def (parse-json-file file <-json (description #f))
  (parse-file file (compose <-json json<-port) description))

;; TODO: have a strict mode in std/text/json that will reject (string<-json (hash ("a" 1) (a 2)))
