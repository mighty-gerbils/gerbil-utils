;; -*- Gerbil -*-
;; Path configuration.

(export #t)

(import
  :std/sugar
  ./base ./config ./filesystem ./path)

;; These paths should be defined or redefined somewhere in your application,
;; typically with e.g.
;;   (set! application-source-envvar "MY_APP_SOURCE")
;;   (set! application-home-envvar "MY_APP_HOME")
(def application-source-envvar (values "GERBIL_APPLICATION_SOURCE"))
(def application-home-envvar (values "GERBIL_APPLICATION_HOME"))
(def source-directory (values (λ () (getenv application-source-envvar))))
(def home-directory (values (λ () (getenv application-home-envvar))))
(def bin-directory (values (λ () (path-expand ".build_outputs" (source-directory))))) ;; executables
(def cache-directory (values (λ () (path-expand "cache" (home-directory))))) ;; dynamic cache
(def config-directory (values (λ () (path-expand "config" (home-directory))))) ;; configuration files
(def data-directory (values (λ () (path-expand "data" (home-directory))))) ;; static data
(def run-directory (values (λ () (path-expand "run" (home-directory))))) ;; transient state
(def log-directory (values (λ () (path-expand "log" (run-directory))))) ;; logs

;; TODO: maybe have a notion of "first to post, then immutable" variable,
;; and of "if not defined yet, try this, then some default, or some default, then this".
;; Then have the API to our configuration values be a function that before/after such defaults,
;; or registers them for future evaluation?
(def (home-directory-default! thunk)
  (try (home-directory) (catch (_) (let (home (thunk)) (set! home-directory (lambda () home))))))
(def (source-directory-default! thunk)
  (try (source-directory) (catch (_) (let (source (thunk)) (set! source-directory (lambda () source))))))

(def (bin-path . x) (apply subpath (bin-directory) x))
(def (cache-path . x) (apply subpath (cache-directory) x))
(def (config-path . x) (apply subpath (config-directory) x))
(def (data-path . x) (apply subpath (data-directory) x))
(def (run-path . x) (apply subpath (run-directory) x))
(def (log-path . x) (apply subpath (log-directory) x))
(def (source-path . x) (apply subpath (source-directory) x))
