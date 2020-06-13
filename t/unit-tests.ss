(import
  :gerbil/gambit/ports
  :clan/utils/exit :clan/utils/path :clan/utils/path-config :clan/utils/ports :clan/utils/source
  ./test-support)

(set-current-ports-encoding-standard-unix!)

(def build-time-clan-src (path-parent (path-normalized-directory (this-source-file))))
(def (clan-src) (getenv "CLAN_SRC" build-time-clan-src))
(def (clan-home) (or (getenv "CLAN_HOME" #f) (clan-src)))
(set! source-directory clan-src)
(set! home-directory clan-home)
(set! config-directory (lambda () (path-expand "etc" (home-directory))))


(def (main . args)
  (eval-print-exit
   (silent-exit
    (match args
      ([] (run-tests "."))
      (["meta"] (println "meta all test process pass"))
      (["all"] (run-tests "." test-files: (find-test-files ".")))
      (["integration"] (run-tests "." test-files: (find-test-files "." "-integrationtest.ss$")))
      (["test" . files] (run-tests "." test-files: files))))))
