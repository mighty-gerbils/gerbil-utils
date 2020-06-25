;; Support for Unix (or Windows?) shells

(export #t)

(import
  :std/srfi/13
  ./base ./basic-parsers)

(def (easy-shell-character? x)
  (or (ascii-alphanumeric? x) (string-index "+-_.,%@:/=" x)))

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
