;;;; Support for simpler ./build.ss scripts
(export #t)

(import
  (only-in :gerbil/gambit pretty-print shell-command)
  (only-in :std/cli/getopt flag)
  (only-in :std/cli/multicall define-entry-point set-default-entry-point!
           current-program define-multicall-main)
  #;(only-in :std/error dump-stack-trace?) ;; only in v0.19
  (only-in :std/make make)
  (only-in :std/source this-source-file)
  (only-in :std/misc/list when/list)
  (only-in :std/misc/path path-maybe-normalize subpath path-extension-is? path-default-extension)
  (only-in :std/misc/string string-trim-eol)
  (only-in :std/srfi/1 lset-difference)
  (only-in ./filesystem find-files path-is-script?)
  (only-in ./git-fu update-version-from-git)
  (only-in ./nix-fu gerbil-is-nix?)
  (only-in ./path-config set-path-config-root!
           application-source-directory application-home-directory)
  (only-in ./ports set-current-ports-encoding-standard-unix!))

(def default-exclude '("main.ss" "manifest.ss"))
(def default-exclude-dirs '("run" "t" ".git" "_darcs" ".gerbil"))

(def (all-gerbil-modules exclude: (exclude default-exclude)
                         exclude-dirs: (exclude-dirs default-exclude-dirs))
  ((cut lset-difference equal? <> exclude)
   (find-files "" (lambda (x) (and (path-extension-is? x ".ss") (not (path-is-script? x))))
               recurse?: (lambda (x) (not (member (path-strip-directory x) exclude-dirs))))))

(def (source-file-matcher file (ext ".ss"))
  (def f (path-default-extension file ext))
  (def s (path-strip-extension f))
  (lambda (g) (or (equal? f g) (equal? s g))))

(def (remove-build-file files file) ;; TODO: handle foo vs foo.ss ?
  (def f? (source-file-matcher file))
  (filter (match <>
            ((? f?) #f)
            ([gxc: (? f?) . _] #f)
            (_ #t)) files))

(def (add-build-options files file . options)
  (cons (cons* gxc: file options) (remove-build-file files file)))

(def %name #f)
(def %repo #f)
(def %version-path #f)
(def %deps '())
(def %srcdir #f)
(def %spec all-gerbil-modules)
(def %pkg-config-libs #f)
(def %nix-deps #f)

(def (%set-build-environment!
      script-path
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
  (add-load-path! %srcdir)
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

(defrules init-build-environment! ()
  ((ctx args ...)
   (begin
     (def here (this-source-file ctx))
     ;;(with-id ctx (main) (def main call-entry-point))
     (define-multicall-main ctx)
     (%set-build-environment! here args ...))))

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
    ([static-include: file] x)
    ([(? (cut member <> '(gxc: gsc: exe: static-exe:))) . _] (append x gsc-options))))

(def (build-spec tcc: (tcc #f) optimize: (optimize #f))
  (def gsc-options (make-gsc-options tcc: tcc optimize: optimize
                                     pkg-config-libs: %pkg-config-libs nix-deps: %nix-deps))
  (def files (%spec))
  (map (cut normalize-spec <> gsc-options) files))

(def compile-getopt
  [(flag 'verbose "-V" "--verbose"
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

#;(dump-stack-trace? #f) ;; Only in v0.19

