#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Utilities to run a command in random order

(export #t)

(import
  :std/cli/getopt
  :std/cli/multicall
  :std/format :std/logger :std/iter
  :std/misc/list :std/misc/process :std/srfi/1 :std/sugar
  :clan/base :clan/error :clan/list
  :clan/filesystem :clan/random)

(define-entry-point (random-run . arguments)
  (help: "Run a command with arguments in random order"
   getopt: [(rest-arguments 'arguments help: "arguments to shuffle")])
  (def gopt
    (getopt
     (option 'log "-l" "--log" default: #f help: "path to random-run log")
     (option 'number-at-once "-n" "--number-at-once" default: #f help: "number of arguments at once")
     (option 'files? "-f" "--files" default: #f help: "search for files in listed directories")
     (option 'regex "-r" "--regex" default: ".*" help: "regexp when using file search")
     (rest-arguments 'arguments help: "arguments, followed by -- then random arguments")))
  (start-logger!)
  (try
   (let* ((opt (getopt-parse gopt arguments))
          (log (hash-get opt 'log))
          (number-at-once (hash-get opt 'number-at-once))
          (files? (hash-get opt 'files?))
          (regex (hash-get opt 'regex))
          (arguments (hash-get opt 'arguments))
          (pos (list-index (looking-for "--") arguments))
          (_ (unless pos (abort! 2 "Missing -- delimiter among arguments")))
          (prefix (take arguments pos))
          (rest (drop arguments (+ pos 1)))
          (arguments-to-randomize
           (if files?
             (find-regexp-files rest regex)
             rest))
          (randomized-arguments (shuffle-list arguments-to-randomize))
          (do-it (λ (logger)
                   (for (args (if number-at-once
                                (group-n-consecutive number-at-once randomized-arguments)
                                [randomized-arguments]))
                     (let ((command (append prefix args)))
                       (logger command)
                       (run-process/batch command))))))
     (if log
       (call-with-output-file [path: log create: #t append: #t]
         (λ (f) (do-it (λ (command) (fprintf f "~S~%" command)))))
       (do-it void)))
   (catch (getopt-error? exn)
     (getopt-display-help exn "random-run" (current-error-port))
     (exit 1))
   (catch (exn)
     (display-exception exn (current-error-port))
     (exit 1))))

(def main random-run)
