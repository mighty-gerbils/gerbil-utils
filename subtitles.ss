;; -*- Gerbil -*-
;;;; Parsing, printing and manipulating .srt subtitle files

(export #t)

(import
  :gerbil/gambit
  :std/srfi/141 :scheme/char :std/parser/base
  :std/assert :std/error :std/misc/list :std/misc/number :std/misc/string
  :std/parser/ll1
  :std/srfi/13 :std/sugar
  :std/io :std/text/basic-printers
  ./base ./files)

(def (ll1-srt-time-offset reader)
  (def hours ((ll1-n-digits 2) reader))
  ((ll1-char #\:) reader)
  (def minutes ((ll1-n-digits 2) reader))
  (unless (< minutes 60)
    (raise-parse-error 'll1-srt-time-offset "bad minutes" #f))
  ((ll1-char #\:) reader)
  (def seconds ((ll1-n-digits 2) reader))
  (unless (< seconds 60)
    (raise-parse-error 'll1-srt-time-offset "bad minutes" #f))
  ((ll1-char #\,) reader)
  (def milliseconds ((ll1-n-digits 3) reader))
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
  (ll1/string ll1-srt-time-offset s))

(def (string<-srt-time-offset o)
  (call-with-output-string [] (curry display-srt-time-offset o)))

;;; Tests:
;;(assert-equal (srt-time-offset<-string "42:02:41,406") 151361406)
;;(assert-equal (string<-srt-time-offset 151361406) "42:02:41,406")

(def (ll1-srt-entry reader)
  (ll1-skip-space* reader)
  (def id (ll1-uint reader))
  (ll1-eol reader)
  (def start-offset (ll1-srt-time-offset reader))
  ((ll1-string " --> ") reader)
  (def end-offset (ll1-srt-time-offset reader))
  (ll1-eol reader)
  (def text
    (with-list-builder (c)
      (let loop ()
        (let ((l (ll1-line reader)))
          (unless (string-null? l)
            (c l) (loop))))))
  [id start-offset end-offset . text])

(def (parse-srt-port port)
  (def reader (PeekableStringReader (open-buffered-string-reader port)))
  ((ll1-char? (integer->char #xfeff)) reader) ;; Skip any leading UTF-8 BOM.
  (with-list-builder (c)
    (until (peekable-eof? reader)
      (c (ll1-srt-entry reader)))))

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

