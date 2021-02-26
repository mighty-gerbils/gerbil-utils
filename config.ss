;; -*- Gerbil -*-
(export #t)

;; TODO: Maybe merge this file into config.ss?

(import
  :std/iter :std/sugar
  :gerbil/gambit/os
  :std/srfi/1 :std/srfi/13
  ./base ./path)

(def (user-home)
  (or (getenv (cond-expand (windows "DEFAULTUSERPROFILE") (else "HOME")) #f)
      (ignore-errors (user-info-home (user-info (user-name))))
      "/"))

;; 1. On Windows, a native application would use the base directories from the Windows Registry,
;; though important values are exported as environment variables, for scripting convenience.
;; https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables
;; If an application is written by company MegaCorp and is named Foo App, it will store
;; data under (subpath base "MegaCorp" "Foo App" (note the two folders),
;;
;; 2. On macOS it should be com.MegaCorp.Foo-App (invalid characters replaced with -).
;; But preferences would go in a single file in plist XML format in
;; On macOS, a native application would have its preferences in
;; ~/Library/Preferences/name.often.with.domain.myapp.plist
;;
;; 3. Under Linux and other unices, it would use XDG for the base then (subpath base "fooapp")
;; (single subdir, lower-cased, no spaces).
;; On Linux, a modern polite native application would use the XDG Base Directory Specification 0.7
;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
;;
;; NB: Should we reproduce this Python library? https://github.com/ActiveState/appdirs

;; XDG basedir spec: "All paths set in these environment variables must be absolute. If an implementation encounters a relative path in any of these variables it should consider the path invalid and ignore it."
;; Return the content of an environment variable if it's an absolute path.
;; : (OrFalse PathString) <- String
(def (getenv-absolute-path envvar)
  (def val (getenv envvar #f))
  (and (string? val) (path-absolute? val) val))

;; Directory separator for environment variables in the current operating system
;; : Char
(def dirs-separator
  (cond-expand
    (windows #\;)
    (else #\:)))

;; Separate a environment variable value string representing a list of paths
;; into a list of the individual paths.
;; : (Listof String) <- String
(def (split-dirs dirs)
  (string-split dirs dirs-separator))

;; Given an environment variable name, extract the value as a list of absolute paths
;; into a list of the individual absolute paths (filtering out other values),
;; or #f if the variable in not defined.
;; : (OrFalse (Listof PathString)) <- String
(def (getenv-absolute-paths envvar)
  (def val (getenv envvar #f))
  (and (string? val) (filter path-absolute? (split-dirs val))))

(cond-expand
  (windows
   ;; TODO: instead of looking up the shell environment,
   ;; lookup keys under Windows registry under this key path:
   ;; HKEY_USERS\DefaultUser\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders
   ;; Except that many other directories with environment variables have completely different key paths.
   (define (get-folder-path key)
     (getenv-absolute-path (string-upcase key))))
  (else (void)))

(defrule (defxdgdir name envvar forms ...)
  (def (name . more)
    (def base
      (or (getenv-absolute-path envvar)
          (cond-expand forms ...)))
    (and base (apply subpath base more))))

(defrule (defxdgdirs name envvar forms ...)
  (or (getenv-absolute-path envvar)
      (cond-expand forms ...)))

;; Returns an absolute pathname for the directory containing user-specific data files,
;; extended with any specified components.
;; XDG basedir spec: "There is a single base directory relative to which user-specific data files should be written. This directory is defined by the environment variable $XDG_DATA_HOME."
;; "$XDG_DATA_HOME defines the base directory relative to which user specific data files should be stored. If $XDG_DATA_HOME is either not set or empty, a default equal to $HOME/.local/share should be used.
;; PathString <- PathComponentString ...
(defxdgdir xdg-data-home "XDG_DATA_HOME"
  (windows (get-folder-path "LocalAppData"))
  (darwin (subpath (user-home) "Library"))
  (else (subpath (user-home) ".local/share")))

;; Returns a pathname for the directory containing user-specific configuration files.
;; XDG basedir spec: "There is a single base directory relative to which user-specific configuration files should be written. This directory is defined by the environment variable $XDG_CONFIG_HOME."
;; "$XDG_CONFIG_HOME defines the base directory relative to which user specific configuration files should be stored. If $XDG_CONFIG_HOME is either not set or empty, a default equal to $HOME/.config should be used."
;; : PathString <- PathComponentString ...
(defxdgdir xdg-config-home "XDG_CONFIG_HOME"
  (windows (xdg-data-home "config"))
  ;; Native applications would put a file com.domain.name.appname.plist in there;
  ;; but if you're using the XDG convention, you're probably not, so better not pollute that folder?
  ;; (darwin (subpath (user-home) "Library/Preferences"))
  (else (subpath (user-home) ".config")))

;; XDG basedir spec: "There is a set of preference ordered base directories relative to which data files should be searched. This set of directories is defined by the environment variable $XDG_DATA_DIRS."
;; "$XDG_DATA_DIRS defines the preference-ordered set of base directories to search for data files in addition to the $XDG_DATA_HOME base directory. The directories in $XDG_DATA_DIRS should be seperated with a colon ':'."
;; "If $XDG_DATA_DIRS is either not set or empty, a value equal to /usr/local/share/:/usr/share/ should be used."
(defxdgdirs xdg-data-dirs "XDG_DATA_DIRS"
  (windows (map get-folder-path ["AppData" "CommonAppData"]))
  (else '("/usr/local/share/" "/usr/share/")))

;; XDG basedir spec: "There is a set of preference ordered base directories relative to which configuration files should be searched. This set of directories is defined by the environment variable $XDG_CONFIG_DIRS."
;; "$XDG_CONFIG_DIRS defines the preference-ordered set of base directories to search for configuration files in addition to the $XDG_CONFIG_HOME base directory. The directories in $XDG_CONFIG_DIRS should be seperated with a colon ':'."
;; "If $XDG_CONFIG_DIRS is either not set or empty, a value equal to /etc/xdg should be used."
(defxdgdirs xdg-config-dirs "XDG_CONFIG_DIRS"
  (windows (map (cut subpath <> "config") (filter identity (xdg-data-dirs))))
  (else '("/etc/xdg/")))

;; XDG basedir spec: The order of base directories denotes their importance; the first directory listed is the most important. When the same information is defined in multiple places the information defined relative to the more important base directory takes precedent. The base directory defined by $XDG_DATA_HOME is considered more important than any of the base directories defined by $XDG_DATA_DIRS. The base directory defined by $XDG_CONFIG_HOME is considered more important than any of the base directories defined by $XDG_CONFIG_DIRS.
(def (search-xdg-dirs base dirs . more)
  (let/cc return
    (for (d [base . dirs])
      (when d
        (let (p (apply subpath d more))
          (when (file-exists? p) (return p)))))
    #f))
#;
(def (search-data-dirs . more)
  (apply search-xdg-dirs (xdg-data-home) (xdg-data-dirs) more))
#;
(def (search-config-dirs . more)
  (apply search-xdg-dirs (xdg-config-home) (xdg-config-dirs) more))

;; XDG basedir spec: "There is a single base directory relative to which user-specific non-essential (cached) data should be written. This directory is defined by the environment variable $XDG_CACHE_HOME."
;; : PathString <- PathComponentString ...
(defxdgdir xdg-cache-home "XDG_CACHE_HOME"
  (windows (getenv-absolute-path "TEMP"))
  (macosx (subpath (user-home) "Library/Caches"))
  (else (subpath (user-home) ".cache")))

;; XDG basedir spec: "There is a single base directory relative to which user-specific runtime files and other file objects should be placed. This directory is defined by the environment variable $XDG_RUNTIME_DIR."
;; "$XDG_RUNTIME_DIR defines the base directory relative to which user-specific non-essential runtime files and other file objects (such as sockets, named pipes, ...) should be stored. The directory MUST be owned by the user, and he MUST be the only one having read and write access to it. Its Unix access mode MUST be 0700."
;; "The lifetime of the directory MUST be bound to the user being logged in. It MUST be created when the user first logs in and if the user fully logs out the directory MUST be removed. If the user logs in more than once he should get pointed to the same directory, and it is mandatory that the directory continues to exist from his first login to his last logout on the system, and not removed in between. Files in the directory MUST not survive reboot or a full logout/login cycle."
;; "The directory MUST be on a local file system and not shared with any other system. The directory MUST by fully-featured by the standards of the operating system. More specifically, on Unix-like operating systems AF_UNIX sockets, symbolic links, hard links, proper permissions, file locking, sparse files, memory mapping, file change notifications, a reliable hard link count must be supported, and no restrictions on the file name character set should be imposed. Files in this directory MAY be subjected to periodic clean-up. To ensure that your files are not removed, they should have their access time timestamp modified at least once every 6 hours of monotonic time or the 'sticky' bit should be set on the file."
;; "If $XDG_RUNTIME_DIR is not set applications should fall back to a replacement directory with similar capabilities and print a warning message. Applications should use this directory for communication and synchronization purposes and should not place larger files in it, since it might reside in runtime memory and cannot necessarily be swapped out to disk."
(defxdgdir xdg-runtime-dir "XDG_RUNTIME_DIR"
  ;; On Windows or macOS, this is likely to return #f, at which point the spec is specific enough
  ;; that the application should either arrange for its own fallback, or error out
  ;; complaining that there is no XDG_RUNTIME_DIR.
  (else #f))
