;; Support for Unix shells
;; TODO: If Windows shell support is needed, add it here, too.

(export #t)

(import
  :std/srfi/13 :std/stxutil :std/text/char-set
  ./base)

(def (easy-shell-character? x)
  (or (char-ascii-alphanumeric? x) (string-index "%+,-./:=@^_~" x)))

(def (needs-shell-escape? token)
  (not (string-every easy-shell-character? token)))

(def (escape-shell-token token)
  (if (needs-shell-escape? token)
    (call-with-output-string []
     (lambda (port)
       (display #\" port)
       (string-for-each
        (lambda (c) (when (string-index "$`\\\"" c) (display #\\ port)) (display c port))
        token)
       (display #\" port)))
    token))

(def (escape-shell-tokens tokens)
  (string-join (map escape-shell-token tokens) " "))

(def (envvar<- . args)
  (call-with-output-string
   (lambda (p)
     (def alpha? #t)
     (string-for-each
      (lambda (c)
        (def caa? (char-ascii-alphanumeric? c))
        (when caa?
          (unless alpha? (write-char #\_ p))
          (write-char c p))
        (set! alpha? caa?))
      (string-upcase (as-string args))))))
