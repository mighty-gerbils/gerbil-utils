;; -*- Gerbil -*-
;; Path configuration for various application files depending on their life-cycle.
;; Largely based on the XDG Base Directory Specification
;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
;; TODO: Maybe merge config.ss into this file?

(export #t)

(import
  :std/misc/path
  :std/sugar
  ./base ./config ./filesystem ./shell)

(def application-name (values (lambda () "gerbil")))
(def (application-name/) (string-append (application-name) "/"))

;; These paths should be defined or redefined somewhere in your application,
;; typically with e.g.
;;   (set! application-source-envvar "MY_APP_SOURCE")
;;   (set! application-home-envvar "MY_APP_HOME")
(def (application-source-envvar) (envvar<- (application-name) '-source))
(def (application-home-envvar) (envvar<- (application-name) '-home))

;; Default for application-source-directory below when the variable is undefined.
;; : (OrFalse String)
(def default-application-source-directory
  (values #f))

;; Where the top of the application source files are stored.
;; Only meaningful for some applications.
;; : String <-
(def application-source-directory
  (values (cut getenv (application-source-envvar) default-application-source-directory)))

;; Default for application-home-directory below when the variable is undefined.
;; : (OrFalse String)
(def default-application-home-directory
  (values #f))

;; Where the top of the application run files are stored.
;; Only meaningful for some applications.
;; : String <-
(def application-home-directory
  (values (cut getenv (application-home-envvar) default-application-home-directory)))

;; Where executables and other things are created.
;; : String <-
(def build-output-directory
  (values (cut subpath (application-source-directory) ".build_outputs/")))

;; Where dynamic cache data is stored.
;; This directory can be bulky and it's OK not to back it up, as
;; what matters can be reconstituted, and/or will/should be copied over to other directories.
;; : String <-
(def cache-directory
  (values (cut xdg-cache-home (application-name/))))

;; Where configuration files
;; For now, also where the persistent data files are stored by default (under db/)
;; : String <-
(def config-directory
  (values (cut xdg-config-home (application-name/))))

;; Where static application data files are stored.
;; These files are application-controlled rather that e.g. user-configuration.
;; : String <-
(def data-directory
  (values (cut xdg-data-home (application-name/))))

;; Where logs are stored.
;; These files are used for monitoring in case things go wrong, or for performance.
;; They can be periodically compressed, archived and/or purged,
;; but typically may survive the current session.
;; : String <-
(def log-directory
  (values (cut xdg-data-home (application-name/) "log")))

;; Where transient data for the current session is stored.
;; These files are may be purged after the session is over or when the next session starts,
;; but there is no guarantee that it will be purged, either
;; (for that, use xdg-runtime-dir).
;; : String <-
(def transient-directory
  (values (cut subpath (or (xdg-runtime-dir) (subpath (cache-directory) "run")) (application-name/))))

;; Where to store the user's persistent application data that lives from one session to the next
;; : String <-
(def persistent-directory
  (values (cut subpath (config-directory) "db/")))

;; Use a root directory for all path configuration,
;; rather than store it in the user's respective relevant directories.
;; : Unit <- String
(def (set-path-config-root! (dir (subpath (application-home-directory) "run/")))
  (set! build-output-directory (cut subpath dir "build_outputs/"))
  (set! cache-directory (cut subpath dir "cache/"))
  (set! config-directory (cut subpath dir "config/"))
  (set! data-directory (cut subpath dir "data/"))
  (set! log-directory (cut subpath dir "log/"))
  (set! transient-directory (cut subpath dir "session/"))
  (set! persistent-directory (cut subpath dir "db/")))

(def (source-path . x) (apply subpath (application-source-directory) x))
(def (application-path . x) (apply subpath (application-home-directory) x))
(def (build-output-path . x) (apply subpath (build-output-directory) x))
(def (cache-path . x) (apply subpath (cache-directory) x))
(def (config-path . x) (apply subpath (config-directory) x))
(def (data-path . x) (apply subpath (data-directory) x))
(def (log-path . x) (apply subpath (log-directory) x))
(def (transient-path . x) (apply subpath (transient-directory) x))
(def (persistent-path . x) (apply subpath (persistent-directory) x))

;; TODO: Maybe use POO and inheritance to compose configurations?
