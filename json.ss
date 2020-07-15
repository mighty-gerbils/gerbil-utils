;; -*- Gerbil -*-
;;;; Basic helpers for JSON

(export #t)

(import
  :gerbil/gambit/ports
  :std/misc/alist :std/misc/ports :std/misc/plist :std/misc/rtd :std/sort :std/sugar :std/text/json
  ./base ./list ./files ./subprocess)

(def (trivial-json<-object object)
  (match (class->list object)
    ([type . plist]
     (list->hash-table
      `(#|(__class . ,(symbol->string (type-name type)))|# ,@(plist->alist plist))))))
(def (trivial-object<-json klass json)
  (def (find-key s) (or (##find-interned-keyword s) (error "invalid json key for class" s klass)))
  (apply make-class-instance klass (alist->plist (map (cut map/car find-key <>) (hash->list json)))))


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
  (parameterize ((json-symbolic-keys #f)) ;; Don't intern JSON keys
    (string->json-object x)))

(def (string<-json object)
  (parameterize ((json-symbolic-keys #f))
    (json-object->string object)))

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
  (call-with-input-file (cons* path: file settings) read-json))

(def (write-file-json file json . settings)
  (clobber-file file (curry write-json json) settings: settings))

(def (json-normalize x)
  (json<-string (string<-json x)))
