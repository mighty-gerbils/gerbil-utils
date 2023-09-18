(export #t)

(import
  :std/misc/string
  :std/misc/ports
  :std/misc/process)

(def (nix-path package)
  (string-trim-eol
   (run-process
    coprocess: read-all-as-string
    ["nix" "path-info" "-f" "<nixpkgs>" package])))

(def (gerbil-is-nix?)
  (string-prefix? "/nix/store/" (gerbil-home)))
