;; -*- Gerbil -*-
;;;; Run and handle subprocesses

(export #t)

(import
  :gerbil/gambit/ports :gerbil/gambit/threads
  :std/misc/process :std/sugar
  ./base)

;; write data into a filter process and read some data back.
;; process-options as per open-process, except you should only use
;; path: arguments: directory: environment:
(def (filter-with-process command writer reader)
  (run-process
   command
   coprocess:
   (λ (process)
     (spawn/name
      ['writing-to command]
      (λ ()
        (try
         (writer process)
         (force-output process)
         (finally
          (close-output-port process)))))
     (try
      (reader process)
      (finally
       (close-port process))))))

