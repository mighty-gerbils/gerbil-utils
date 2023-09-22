;;;; Support for simpler ./build.ss scripts
(export #t)

(import
  :gerbil/gambit
  :std/format :std/getopt :std/iter :std/make
  :std/misc/list :std/misc/ports :std/misc/process :std/misc/string
  :std/pregexp :std/srfi/1 :std/srfi/13 :std/sugar
  ./exit ./filesystem ./git-fu ./multicall ./nix-fu
  ./path ./path-config ./ports ./source ./versioning)

(def (all-gerbil-modules exclude: (exclude '("main.ss"))
                         exclude-dirs: (exclude-dirs '("run" "t" ".git" "_darcs")))
  ((cut lset-difference equal? <> exclude)
   (find-files "" (lambda (x) (and (path-extension-is? x ".ss") (not (path-is-script? x))))
               recurse?: (lambda (x) (not (member (path-strip-directory x) exclude-dirs))))))

(def %name #f)
(def %repo #f)
(def %version-path #f)
(def %deps '())
(def %srcdir #f)
(def %spec all-gerbil-modules)
(def %pkg-config-libs #f)
(def %nix-deps #f)

(def (%set-build-environment!
      script-path add-load-path
      name: (name #f)
      repo: (repo #f)
      deps: (deps '())
      version-path: (version-path #f)
      spec: (spec #f)
      pkg-config-libs: (pkg-config-libs #f)
      nix-deps: (nix-deps #f))
  (set-current-ports-encoding-standard-unix!)
  (set! %srcdir (path-maybe-normalize (path-directory script-path)))
  (current-directory %srcdir)
  (add-load-path %srcdir)
  (set-path-config-root! (subpath %srcdir "run"))
  (set! application-source-directory (lambda () %srcdir))
  (set! application-home-directory (lambda () %srcdir))
  (set! %name name)
  (set! %repo repo)
  (set! %deps deps)
  (set! %version-path version-path)
  (when spec (set! %spec spec))
  (set! %pkg-config-libs pkg-config-libs)
  (set! %nix-deps nix-deps)
  (set-default-entry-point! 'compile)
  (current-program (path-strip-directory script-path)))

(defrule (%init-build-environment! ctx args ...)
  (begin
    (def here (this-source-file ctx))
    (with-id ctx (main add-loadpath)
      (define-multicall-main ctx)
      (%set-build-environment! here add-load-path args ...))))

(defsyntax (init-build-environment! stx)
  (syntax-case stx () ((ctx args ...) #'(%init-build-environment! ctx args ...))))

(def ($ cmd)
  (match (shell-command cmd #t)
    ([ret . path] (and (zero? ret) (string-trim-eol path)))))
(def (which? cmd) ($ (string-append "which " cmd)))

(def (pkg-config-options pkg-config-libs nix-deps)
  ;; If running a nix gxi from outside a nix build, we'll query nix-shell for pkg-config information
  (def nix-hack? (and (gerbil-is-nix?) (which? "nix-shell")))
  (def ($$ command)
    ($ (if nix-hack?
         (string-append "nix-shell '<nixpkgs>' -p pkg-config " (string-join nix-deps " ")
                        " --run '" command "' 2> /dev/null")
         command)))
  (when nix-hack? ($$ "echo ok")) ;; do a first run to ensure all dependencies are loaded
  (def ($pkg-config options)
    (or ($$ (string-join ["pkg-config" . options] " "))
        (error "Failed to autodetect C library options. Are these libraries installed?"
          pkg-config-libs)))
  ["-ld-options" ($pkg-config ["--libs" . pkg-config-libs])
   "-cc-options" ($pkg-config ["--cflags" . pkg-config-libs])])

(def gsc-options/no-optimize '("-cc-options" "-O0 -U___SINGLE_HOST"))
(def gsc-options/tcc '("-cc" "tcc" "-cc-options" "-shared"))

(def (make-gsc-options tcc: tcc?
                       optimize: optimize?
                       pkg-config-libs: pkg-config-libs
                       nix-deps: nix-deps)
  (append (when/list tcc? gsc-options/tcc)
          (when/list (not optimize?) gsc-options/no-optimize)
          (when/list pkg-config-libs (pkg-config-options pkg-config-libs nix-deps))))

(def (normalize-spec x gsc-options)
  (match x
    ((? string?) [gxc: x . gsc-options])
    ([(? (cut member <> '(gxc: gsc: exe: static-exe:))) . _] (append x gsc-options))))

(def (build-spec tcc: (tcc #f) optimize: (optimize #f))
  (def gsc-options (make-gsc-options tcc: tcc optimize: optimize
                                     pkg-config-libs: %pkg-config-libs nix-deps: %nix-deps))
  (def files (%spec))
  (map (cut normalize-spec <> gsc-options) files))

(def compile-getopt
  [(flag 'verbose "-v" "--verbose"
         help: "Make the build verbose")
   (flag 'debug "-g" "--debug"
         help: "Include debug information")
   (flag 'tcc "-t" "--tcc"
         help: "Use tinycc for a faster compile")
   (flag 'no-optimize "--O" "--no-optimize"
         help: "Disable Gerbil optimization")])

(def (create-version-file)
  (update-version-from-git name: %name deps: %deps path: %version-path repo: %repo))

(define-entry-point (compile verbose: (verbose #f) debug: (debug #f)
                             tcc: (tcc #f) no-optimize: (no-optimize #f))
  (help: "Compile all the files in this package"
   getopt: compile-getopt)
  (def optimize? (not no-optimize))
  (when %name (create-version-file))
  (make (build-spec tcc: tcc optimize: optimize?)
    verbose: (and verbose 9) debug: (and debug 'env) optimize: optimize? srcdir: %srcdir))

(define-entry-point (spec verbose: (verbose #f) debug: (debug #f)
                          tcc: (tcc #f) no-optimize: (no-optimize #f))
  (help: "Show the build specification"
   getopt: compile-getopt)
  (def optimize? (not no-optimize))
  (pretty-print (build-spec tcc: tcc optimize: optimize?)))

(backtrace-on-abort? #f)
