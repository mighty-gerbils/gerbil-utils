;; -*- Gerbil -*-
(export #t)

(import
  :gerbil/gambit/os
  ./base)

(def (user-home)
  (or (getenv "HOME" #f)
      (ignore-errors (user-info-home (user-info (user-name))))
      "/"))
