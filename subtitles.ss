;; -*- Gerbil -*-
;;;; Parsing, printing and manipulating .srt subtitle files

(export #t)

(import
  :gerbil/gambit/exact
  :scheme/base-impl :scheme/char
  :std/error :std/misc/list :std/misc/string :std/srfi/13 :std/sugar
  ./base ./basic-parsers ./files ./number)

(def (expect-srt-time-offset port)
  (def hours ((expect-n-digits 2) port))
  ((expect-char #\:) port)
  (def minutes ((expect-n-digits 2) port))
  (unless (< minutes 60)
    (parse-error! 'expect-srt-time-offset "bad minutes"))
  ((expect-char #\:) port)
  (def seconds ((expect-n-digits 2) port))
  (unless (< seconds 60)
    (parse-error! 'expect-srt-time-offset "bad minutes"))
  ((expect-char #\,) port)
  (def milliseconds ((expect-n-digits 3) port))
  (+ milliseconds (* 1000 (+ seconds (* 60 (+ minutes (* 60 hours)))))))

(def (display-srt-time-offset offset (port (current-output-port)))
  (assert! (exact-integer? offset))
  (defvalues (seconds+ milliseconds) (floor/ offset 1000))
  (defvalues (minutes+ seconds) (floor/ seconds+ 60))
  (defvalues (hours minutes) (floor/ minutes+ 60))
  (assert! (< hours 99))
  (display-integer/fit hours 2 port)
  (display #\: port)
  (display-integer/fit minutes 2 port)
  (display #\: port)
  (display-integer/fit seconds 2 port)
  (display #\, port)
  (display-integer/fit milliseconds 3 port))

(def (srt-time-offset<-string s)
  (call-with-input-string s (λ (p) (begin0 (expect-srt-time-offset p) (expect-eof p)))))

(def (string<-srt-time-offset o)
  (call-with-output-string [] (curry display-srt-time-offset o)))

;;; Tests:
;;(assert-equal (srt-time-offset<-string "42:02:41,406") 151361406)
;;(assert-equal (string<-srt-time-offset 151361406) "42:02:41,406")

(def (parse-srt-entry port)
  (expect-and-skip-any-whitespace port)
  (def id (expect-natural port))
  (expect-eol port)
  (def start-offset (expect-srt-time-offset port))
  ((expect-literal-string " --> ") port)
  (def end-offset (expect-srt-time-offset port))
  (expect-eol port)
  (def text
    (with-list-builder (c)
      (let loop ()
        (let ((l (expect-line port)))
          (unless (or (eof-object? l)
                      (string-null? l))
            (c l) (loop))))))
  [id start-offset end-offset . text])

(def (parse-srt-port port)
  ((expect-maybe-char (integer->char #xfeff)) port) ;; Skip any leading UTF-8 BOM.
  (with-list-builder (c)
    (until (char-port-eof? port)
      (c (parse-srt-entry port)))))

(def (renumber-srt srt)
  (def id 0)
  (with-list-builder (c)
    (for-each!
     srt
     (λ-match
      ([_ start end . text]
       (increment! id)
       (c [id start end . text]))))))

(def (display-crlf port)
  (display +crlf+ port))

(def (display-srt srt (port (current-output-port)))
  (for-each!
   srt
   (λ-match
    ([id start end . text]
     (display id port)
     (display-crlf port)
     (display-srt-time-offset start port)
     (display " --> " port)
     (display-srt-time-offset end port)
     (display-crlf port)
     (for-each! text (λ (x) (display x port) (display-crlf port)))
     (display-crlf port)))))

(def (parse-srt-file x . options)
  (call-with-input-file [path: x . options]
    parse-srt-port))

(def (map-srt-file fun file . options)
  (maybe-replace-file
   file (λ (srt) (map fun (renumber-srt srt)))
   reader: parse-srt-port
   writer: display-srt
   settings: options))

(def (srt-time-shift-entry offset e)
  (match e
    ([id start end . text] [id (+ start offset) (+ end offset) . text])))

(def (srt-time-shifter offset (id-start #f) (id-end #f))
  (λ (e)
    (let ((id (car e)))
      (if (and (or (not id-start) (<= id-start id))
               (or (not id-end) (< id id-end)))
        (srt-time-shift-entry offset e)
        e))))

