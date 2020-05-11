(import
  :gerbil/gambit/ports
  :clan/utils/exit :clan/utils/ports
  ./test-support)

(set-current-ports-encoding-standard-unix!)

(def (main . args)
  (eval-print-exit
   (silent-exit
    (match args
      ([] (run-tests "."))
      (["meta"] (println "meta all test process pass"))
      (["all"] (run-tests "." (find-test-files ".")))
      (["test" . files] (run-tests "." files))))))
