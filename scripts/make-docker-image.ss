#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Make docker images for Gerbil, etc.
;;;; Note: you may first need to `docker login registry.gitlab.com`
(export #t)

(import
  :gerbil/gambit
  :std/crypto/digest
  :std/format
  :std/getopt
  :std/iter
  :std/misc/list
  :std/misc/path
  :std/misc/ports
  :std/misc/process
  :std/misc/string
  :std/pregexp
  :std/sort
  :std/source
  :std/srfi/1
  :std/srfi/13
  :std/sugar
  :std/text/hex
  :clan/base :clan/cli :clan/config :clan/debug :clan/exit :clan/files :clan/memo :clan/multicall
  :clan/path-config :clan/ports :clan/shell :clan/string :clan/syntax
  :clan/poo/cli)

(set! application-name (lambda () "make-docker-image"))

;; Default nixpkgs from which to build and install packages.
;; We choose "<nixpkgs>" for your locally configured default;
;; an alternative might be "https://github.com/MuKnIO/nixpkgs/archive/devel.tar.gz"
(def default-nixpkgs "<nixpkgs>")
;; NB: *if* we copy nixpkgs onto the image, we careful to not copy 2GB of disk space from .git,
;; but instead copy a worktree or a tarball.
;; Also remember the origin and git revision/description somehow.

;; Mixins for command-line options
(def options/nixpkgs
  (make-options
   [(optional-argument 'nixpkgs
                       help: "location of URL for nixpkgs"
                       default: default-nixpkgs)] []))
(def options/packages
  (make-options
   [(rest-arguments 'packages help: "Nix packages to install before that command")] []))
(def options/nixpkgs+packages
  (make-options [] [] [options/packages options/nixpkgs]))

;; Extract SHA256 digest from nixpkgs URL.
;; TODO: support local directory as well as remote tarball
(define-memo-function (nixpkgs-digest (nixpkgs default-nixpkgs))
  (sha256<-bytes (http-get-content nixpkgs)))

;; A list of nixpkgs packages needed for convenience on the docker image
;; : (List String)
(def user-packages
  (map stringify
    '(zsh su screen less git xz nix bashInteractive ;; interactive environment
      coreutils attr findutils diffutils patch gnused gnumake ;; interactive utilities
      curl openssh rsync cacert ;; basic networking
      racket multimarkdown ;; needed by documentation generation
      go-ethereum solc ;; needed by gerbil-ethereum
      go-libp2p-daemon ;; needed by gerbil-libp2p
      unzip patchelf zlib.dev ;; will be downloaded during testing if not present here(!?)
      strace man ;; debugging help
      )))

;; These are still somehow downloaded during the Glow build&test even if we explicitly include them,
;; but in a different configuration than is available via nixpkgs. WTF???
(def extra-packages
  (map stringify
    '(stdenv
      openssl.out openssl.dev openssl.bin libressl libressl.dev libressl.man
      curl.dev curl.man nghttp2 nghttp2.lib nghttp2.dev
      bashInteractive.dev bashInteractive.man
      libssh2 libssh2.dev libkrb5 libkrb5.dev
      zsh.man attr.man gnumake.man xz.man openssl.man
    )))

(def all-target-packages
  [user-packages ... extra-packages ...
   "gambit-unstable" "gerbil-unstable" "gerbilPackages-unstable"])

;; Initialize paths from the environment
(def here (this-source-directory))
(set-path-config-root! (subpath (path-parent here) "run"))

(def docker-directory (cache-path "docker"))

;; : String <- (List String)
(def (space-join x) (string-join x " "))

;; : String <- (List String)
(def (line-join x) (string-join x "\n"))

;; : Bytes32 <- Bytes
(def (sha256<-bytes b (start 0) (end (u8vector-length b)))
  (unless (and (u8vector? b) (<= 0 start end (u8vector-length b))) (error 'sha256 b start end))
  (def d (make-digest digest::sha256))
  (digest-update! d b start end)
  (digest-final! d))

;; : String <- (List String)
(def (digest-spec spec) (hex-encode (sha256<-bytes (string->bytes (line-join spec)))))

;; : (List String) <- (List String)
(def (nix-dependencies packages)
  (if (null? packages) []
      (run-process coprocess: read-all-as-lines ["nix" "path-info" "--recursive" packages ...])))

(define-entry-point (clean-docker-directory)
  (help: "Remove the docker directory"
   getopt: [])
  (unless (string-suffix? "/cache/docker" docker-directory)
    (error "Not removing fishy docker-directory" docker-directory))
  (run-process/batch ["chmod" "-R" "u+w" docker-directory] check-status: #f)
  (run-process/batch ["rm" "-rf" docker-directory]))

(define-entry-point (docker-image-id name)
  (help: "Extract image ID for a docker image of a given name"
   getopt: (make-options [(argument 'name)]))
  (match (run-process ["docker" "image" "ls" name] coprocess: read-all-as-lines)
    ([header line]
     (third (remove string-empty? (string-split line #\space))))
    (else #f)))

(def (docker-build-image from tag . commands)
  (create-directory* docker-directory)
  (clobber-file
   (path-expand "Dockerfile" docker-directory)
   (lambda (port)
     (fprintf port "FROM ~a\n" from)
     (for (c commands) (output-contents c port) (newline port))))
  (run-process/batch ["docker" "build" (when/list tag ["--tag" tag])... docker-directory]))

;; Many ways to install a package
;; - Just COPY the data, install, then delete... but then data appears in the history
;;   and makes the image much bigger.
;; - Do things inefficiently as above into an intermediate image, then use docker COPY --from
;;   to efficiently transfer data into the actual image, including an overwrite of the sqlite db.
;; - You cannot mount, but can connect to a server (cachix) to hold the files you need
;; - Use docker run and docker commit to circumvent the lack of mounting at build time
;;     tmp_img=`docker build .`
;;     tmp_container=`docker run -d -v $my_ext_volume:$my_mount_path --entrypoint=(your volume-dependent build command here) $tmp_img`
;;     docker commit $tmp_container my_repo/image_tag:latest
;;     docker rm "$tmp_container"
;;   ^ THIS IS THE METHOD WE USE.
;; - Use podman.io, that is a like docker but can mount at build time.
;; - Use buildah.io, that can build images with mounts
;;     buildah bud --volume /home/test:/myvol:ro -t imageName .
;;
;; Notes for cleanup
;; - To stop all containers, remove them, and prune the images:
;;     containers=($(docker container ls -a | cut -c140- | (read ; cat)))
;;     docker container stop $containers
;;     docker container rm $containers
;;     docker image prune -a
;;
(def (docker-commit-image from to mounts: (mounts []) changes: (changes []) . commands)
  (def build-name (string-substitute #\- #\/ to))
  (def build-script (subpath docker-directory (string-append build-name ".sh")))
  (unless (docker-image-id to)
    (create-directory* docker-directory)
    (clobber-file build-script
                  (line-join ["#!/bin/sh -ex" "echo $0 $*" commands ... ""]))
    (run-process/batch ["chmod" "a+x" build-script])
    (let (container
          (string-trim-eol
           (run-process ["docker" "run" "-d"
                         "--mount" "type=tmpfs,dst=/tmp"
                         (append-map (match <> ([outside inside]
                                                ["--mount"
                                                 (format "type=bind,src=~a,dst=~a,ro=1"
                                                         outside inside)]))
                                     [[docker-directory "/tmp/docker"] mounts ...]) ...
                         from
                         (format "/tmp/docker/~a.sh" build-name)])))
      (run-process/batch ["docker" "wait" container])
      (run-process/batch ["docker" "commit"
                          "--message" to
                          (if (null? changes) [] [(line-join ["--change=" changes ...])]) ...
                          container to])
      (run-process/batch ["docker" "container" "rm" container]))
    (delete-file build-script))
  to)

;; Build a nix image from the image with given "from" tag,
;; to an image with "to" tag suffixed by a hash (unless it already exists),
;; based on the nix paths and commands, *assuming the nix packages were built locally already*.
(def (build-nix-image from to changes: (changes []) paths install: (install paths) . commands)
  (def hash (digest-spec [from to "" install ... "" paths ... "" commands ...]))
  (def tag (string-append to "--" hash))
  (def dependencies (nix-dependencies paths))
  (unless (docker-image-id tag)
    (apply docker-commit-image from tag
           mounts: [["/nix/store" "/tmp/nix-store"]]
           changes: changes
           (if (null? dependencies) ""
               (space-join ["cd /tmp/nix-store ;"
                            "for i in" (map path-strip-directory dependencies) ... ";"
                            "do [ -x /nix/store/$i ] || cp -a $i /nix/store/ ; done"]))
           (if (null? install) ""
               (space-join ["nix-env --show-trace -i" install ...]))
           commands))
  tag)

;; Use alpine as the basis for the installation, because the nixos/nix image somehow has nix and
;; nix-env binaries that immediately fail with "error: Operation not permitted" when invoked(!)
(define-entry-point (make-nix-image (nixpkgs default-nixpkgs))
  (help: "Create a docker image for a minimal nix on top of alpine linux"
   getopt: options/nixpkgs)
  (def packages (nix-paths nixpkgs: nixpkgs "nix"))
  (def nix (last packages))
  (def dependencies (nix-dependencies packages))
  (build-nix-image "library/alpine" "mukn/nix" [] ;; install packages manually
    changes: ["ENTRYPOINT []"
             "CMD [\"/bin/sh\"]"
             "WORKDIR /root"
             "ENV PATH /root/.nix-profile/bin:/root/.nix-profile/sbin:/run/wrappers/bin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/run/current-system/sw/bin:/run/current-system/sw/sbin:/nix/var/nix/profiles/system/sw/bin:/nix/var/nix/profiles/system/sw/sbin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
             "ENV NIX_PATH nixpkgs=/root/nixpkgs:/root/nixpkgs"]
    install: [] ;; install manually because nix-env isn't in the path yet
    "mkdir -p /nix/store /root/.config/nix"
    "cd /tmp/nix-store"
    (space-join ["cp -a" (map path-strip-directory dependencies) ... "/nix/store/"])
    (space-join
     ["(echo sandbox = false"
      ";"
      "echo substituters ="
      "https://cache.nixos.org"
      "https://hydra.iohk.io"
      "https://iohk.cachix.org"
      "https://nixcache.reflex-frp.org"
      "https://mukn.cachix.org"
      ";"
      "echo trusted-public-keys ="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "mukn.cachix.org-1:ujoZLZMpGNQMeZbLBxmOcO7aj+7E5XSnZxwFpuhhsqs="
      ") > /root/.config/nix/nix.conf"])
    "addgroup -S nixbld"
    (space-join ["for i in $(seq -w 1 10); do"
                 "adduser -G nixbld -h /var/empty -s \"$(which nologin)\""
                 "-g \"Nix build user $i\" -S \"nixbld${i}\";"
                 "done"])
    ;; Initialize the nix environment from nix
    (format "~a/bin/nix-env -i ~a" nix (space-join packages))))

;; Return the nix store paths for the given expression;
;; also include the recursive dependencies if recursive? is true.
;; Assumes the packages have already been installed.
(def (nix-paths nixpkgs: (nixpkgs default-nixpkgs) recursive?: (recursive? #f)
                . package-expressions)
  #;(unless (equal? nixpkgs "<nixpkgs>") (setenv "NIX_PATH" (format "nixpkgs=~a" nixpkgs)))
  (run-process
   coprocess: read-all-as-lines
   ["nix" "path-info"
    (if recursive? ["--recursive"] [])... "--impure" "--expr"
    (space-join ["with import" nixpkgs "{}; [" package-expressions ... "]"])]))

(define-entry-point (all-target-paths (nixpkgs default-nixpkgs))
  (help: "Display all nix paths to be installed"
   getopt: options/nixpkgs)
  (apply nix-paths nixpkgs: nixpkgs recursive?: #t all-target-packages))

(define-entry-point (install-command (nixpkgs default-nixpkgs) . package-expressions)
  (help: "Create a shell command to install given packages"
   getopt: options/nixpkgs+packages)
  ["nix-build" "-E" (space-join ["with import" nixpkgs "{}; [" package-expressions ... "]"])])

(define-entry-point (build-all-targets (nixpkgs default-nixpkgs))
  (help: "Create a shell command to install all gerbil-related packages"
   getopt: options/nixpkgs)
  (create-directory* docker-directory)
  (def all-paths
    (run-process directory: docker-directory coprocess: read-all-as-lines
                 (apply install-command nixpkgs all-target-packages)))
  (run-process/batch ["cachix" "push" "mukn" all-paths ...]))

(def (get-nixpkgs nixpkgs: (nixpkgs default-nixpkgs) parent)
  (def origin (subpath parent "nixpkgs" ".origin"))
  (cond
   ((any (cut string-prefix? <> nixpkgs) '("http://" "https://"))
    ;; download with curl, unpack a tarball that has everything in a single directory
    (let (tmp (subpath parent "tmp"))
      (create-directory* tmp)
      (run-process/batch
       ["sh" "-c" (string-append "curl -L " (escape-shell-token nixpkgs) " | tar xf-")]
       directory: tmp)
      (def files (directory-files tmp))
      (unless (and (length=n? files 1) (path-is-directory? (first files)))
        (error "not a tarball containing exactly one directory" nixpkgs))
      (rename-file (subpath tmp (first files)) (subpath parent "nixpkgs"))
      (clobber-file origin (string-append "url: " nixpkgs "\n"))
      (delete-directory tmp)))
   ((string-prefix? "channel:" nixpkgs)
    (let (url (string-append "https://nixos.org/channels/"
                             (string-trim-prefix "channel:" nixpkgs)
                             "/nixexprs.tar.xz"))
      (get-nixpkgs nixpkgs: url parent)))
   ((pregexp-match "^<(.*)>$" nixpkgs) =>
    (lambda (m)
      (def name (second m))
      (def prefix (string-append name "="))
      (let/cc return
        (for (d (string-split (getenv "NIX_PATH" "") #\:))
          (when (string-prefix? prefix d)
            (return (get-nixpkgs nixpkgs: (string-trim-prefix prefix d) parent))))
        (error "Couldn't find nixpkgs in NIX_PATH" name))))
   (else
    (run-process/batch
     ["rsync" "-a" "--exclude" ".git"
      (string-append (path-simplify nixpkgs) "/") (subpath parent "nixpkgs/")])
    (let (git (subpath nixpkgs ".git"))
      (if (file-exists? git)
        (let (description (string-trim-eol (run-process directory: nixpkgs ["git" "describe" "--tags"])))
          (clobber-file origin (string-append "git: " description "\n")))
        (clobber-file origin "unknown\n"))))))

(define-entry-point (make-images (nixpkgs default-nixpkgs))
  (help: "Create image for all packages before gambit"
   getopt: options/nixpkgs)
  ;; 1. Install all the required packages
  (build-all-targets)
  ;; 2. Extract the paths we want
  (def all-paths (all-target-paths nixpkgs))
  (def user-paths (apply nix-paths nixpkgs: nixpkgs user-packages))
  (def gambit-paths (nix-paths nixpkgs: nixpkgs "gambit-unstable"))
  (def gerbil-paths (nix-paths nixpkgs: nixpkgs "gerbil-unstable"))
  (def gerbil-package-paths (nix-paths nixpkgs: nixpkgs "gerbilPackages-unstable"))
  (def dependency-paths
    (lset-difference equal? all-paths (append gambit-paths gerbil-paths gerbil-package-paths)))
  ;; 3. Create successive images
  (def mukn/nix (make-nix-image nixpkgs))
  (def mukn/dependencies (build-nix-image mukn/nix "mukn/dependencies"
                                          install: user-paths dependency-paths))
  (def mukn/gambit (build-nix-image mukn/dependencies "mukn/gambit"
                                    changes: ["ENV GAMBOPT t8,f8,-8,i8,dRr"] gambit-paths
                                    "gsi -e '(display (* 6 7)) (newline)'"))
  (def mukn/gerbil (build-nix-image mukn/gambit "mukn/gerbil"
                                    changes: ["ENV GERBIL_LOADPATH /root/.nix-profile/gerbil/lib"]
                                    gerbil-paths
                                    "gxi -e '(displayln (* 6 7))'"))
  (def mukn/glow (build-nix-image mukn/gerbil "mukn/glow"
                                  gerbil-package-paths))
  ;; 4. Install the nixpkgs so you can nix-env install from within the image
  (clean-docker-directory)
  (create-directory* docker-directory)
  (get-nixpkgs docker-directory)
  (docker-build-image mukn/glow "mukn/glow:devel"
                      "COPY nixpkgs /root/nixpkgs"
                      "ENTRYPOINT []"
                      "CMD [\"/bin/sh\"]")
  ;;(run-process/batch ["docker" "tag" mukn/glow ])
  (def images [mukn/glow mukn/gerbil mukn/gambit mukn/dependencies mukn/nix])

  ;; 5. Erase the docker directory
  (clean-docker-directory)
  ;; 6. Do integration tests
  ;; TODO: assume there is a recent checkout of glow and actually run ci.ss for it with this image?
  ;; or make sure ci.ss is part of the glow nix package and use it from there somehow?
  (def integration-test?
    (let/cc return (run-process/batch
                    ["true"] ;; Note: this is a stub. Replace it with something real!
                    check-status: (lambda (status settings) (return status)))))
  (unless integration-test?
    (error "Integration test failed"))
  ;; 9. Push the docker image
  (run-process/batch ["docker" "push" "mukn/glow:devel"])
  (newline)
  (apply values images))

(define-entry-point (gc-containers . containers)
  (help: "Purge all containers except specified ones"
   getopt: (make-options [(rest-arguments 'containers)]))
  (def good (if (null? containers) '()
                (map path-strip-directory
                     (run-process coprocess: read-all-as-lines
                                  ["docker" "container" "inspect" "--format" "{{.Name}}" containers ...]))))
  (def all (run-process coprocess: read-all-as-lines
                        ["docker" "container" "ls" "-a" "--format" "{{.Names}}"]))
  (def bad (lset-difference equal? all good))
  (unless (null? bad)
    (run-process/batch check-status: #f ["docker" "container" "kill" bad ...])
    (run-process/batch check-status: #f ["docker" "container" "rm" bad ...])))

(define-entry-point (gc-images . images)
  (help: "Purge all images except specified ones"
   getopt: (make-options [(rest-arguments 'images)]))
  ;; Start dummy containers for all images we want to preserve
  (def containers
    (map (lambda (i) (substring (run-process ["docker" "run" "-d" i "sleep" "999999999"]) 0 12)) images))
  (run-process/batch check-status: #f ["docker" "image" "prune" "-a" "-f"])
  (unless (null? containers)
    (run-process/batch check-status: #f ["docker" "container" "stop" "-t" "0" containers ...])
    (run-process/batch check-status: #f ["docker" "container" "rm" containers ...])))

(define-entry-point (all (nixpkgs default-nixpkgs))
  (help: "Rebuild all images"
   getopt: options/nixpkgs)
  (make-images nixpkgs))

(set-default-entry-point! 'all)
(backtrace-on-abort? #f)
(define-multicall-main)

#|
# To make a release:
pushtag () { for i ; do docker tag mukn/glow mukn/glow:$i ; docker push mukn/glow:$i ; done; }
pushtag latest devel

# Cleanup images:
docker image rm --force $(docker image ls | (read ; grep -v alpine) | (while read r t i c s ; do echo $i ; done)) ; docker image ls
|#
