;; -*- Gerbil -*-
;;;; Basic helpers for JSON

(export #t)

(import
  :gerbil/gambit/ports
  :std/misc/ports :std/misc/rtd :std/text/json
  :utils/base :utils/files :utils/subprocess)

(def (trivial-json<-object object)
  (match (class->list object)
    ([type . plist]
     (list->hash-table
      `((__class . ,(symbol->string (type-name type))) ,@plist)))))

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

(def (<-json string)
  (parameterize ((json-symbolic-keys #f))
    (string->json-object string)))

(def (json<- object)
  (parameterize ((json-symbolic-keys #f))
    (json-object->string object)))

;; For better performance when skipping, parse json lazily.
(def (lazy<-json string (decode identity))
  (delay
    (decode
     (begin0 (<-json string)
       (set! string #f))))) ;; reclaim storage (or will gambit do it based on lifespan analysis?)

(def (expect-lazy-json-line port (decode identity))
  (lazy<-json (read-line port)))

(def (read-file-json file . settings)
  (call-with-input-file (cons* path: file settings) read-json))

(def (write-file-json file json . settings)
  (clobber-file file (curry write-json json) settings: settings))
