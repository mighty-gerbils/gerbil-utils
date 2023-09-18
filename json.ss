;; -*- Gerbil -*-
;;;; Basic helpers for JSON

(export #t)

(import
  :gerbil/gambit/ports
  :std/iter :std/misc/alist :std/misc/hash :std/misc/list-builder
  :std/misc/ports :std/misc/plist :std/misc/rtd :std/misc/walist
  :std/sort :std/srfi/43 :std/sugar :std/text/json :std/text/basic-parsers
  ./base ./list ./files)

(def (string<-json object)
  (parameterize ((json-sort-keys #t))
    (json-object->string object)))

(def (json<-string str)
  (parameterize ((json-symbolic-keys #f))
    (string->json-object str)))

;; TODO: rename to safe-read-json or read-json/string-keys or something
(def (json<-port port)
  (parameterize ((json-symbolic-keys #f))
    (port->json-object port)))

;; For better performance when skipping, parse json lazily.
(def (lazy-json<-string string (decode identity))
  (delay
    (decode
     (begin0 (json<-string string)
       (set! string #f))))) ;; reclaim storage (or will gambit do it based on lifespan analysis?)

(def (read-lazy-json-line port (decode identity))
  (lazy-json<-string (read-line port)))

(def (read-file-json file . settings)
  (call-with-input-file (cons* path: file settings) json<-port))

(def (write-file-json file json . settings)
  (parameterize ((json-sort-keys #t))
    (clobber-file file (curry write-json json) settings: settings)))

(def (json-normalize x)
  (json<-string (string<-json x)))

(def (write-json-ln x (port (current-output-port)))
  (parameterize ((json-sort-keys #t))
    (write-json x port) (newline port)))

(def (parse-json-file <-json file)
  (<-json (call-with-input-file file port->json-object)))

;; json-key=? : StringOrSymbol StringOrSymbol -> Bool
(def (json-key=? a b)
  (cond ((and (string? a) (string? b)) (equal? a b))
        ((and (symbol? a) (symbol? b)) (eq? a b))
        ((and (string? a) (symbol? b)) (equal? a (symbol->string b)))
        ((and (symbol? a) (string? b)) (equal? (symbol->string a) b))
        (else (error "json-key=?: expected strings or symbols, given" a b))))

;; json-object-ref : JsonObject StringOrSymbol [-> Json] -> Json
(def (json-object-ref j k (d (cut error "json-object-ref: No value associated with key" j k)))
  ;; toggle : StringOrSymbol -> StringOrSymbol
  (def (toggle s)
    (cond ((symbol? s) (symbol->string s))
          (else        (string->symbol s))))
  (cond
    ((hash-table? j)
     (hash-ref/default j k
       (lambda () (hash-ref/default j (toggle k) d))))
    ((walist? j)
     (let ((e (assoc k (walist->alist j) json-key=?)))
       (if e (cdr e) (d))))
    (else (error "json-object-ref: expected a hash-table or Alist struct, given" j))))

;; json-object-get : JsonObject StringOrSymbol Json -> Json
(def (json-object-get j k (d #f))
  (json-object-ref j k (lambda () d)))
