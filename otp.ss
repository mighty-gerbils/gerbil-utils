;; OTP support using oathtool
#|
;; Install oathtool with a command such as:
apt install oathtool
nix-env -f '<nixpkgs>' -iA oath-toolkit

;; Create your personal executable otp script with content such as below:
#!/usr/bin/env gxi
(export #t)
(import :clan/otp)
(def main otp)
(register-otp-keys
  myaccount@gmail.com: "abcd efgh ijkl mnop qrst uvwx yz12 3456"
  myaccount@github.com: "0123456789ABCDEF")
|#

(export #t)

(import
  (only-in :std/cli/getopt optional-argument)
  (only-in :std/cli/multicall define-entry-point)
  (only-in :std/format printf)
  (only-in :std/misc/alist plist->alist)
  (only-in :std/misc/process run-process)
  (only-in :std/srfi/1 second))

(def otp-keys (values []))

(def (generate-otp key)
  (run-process ["oathtool" "--totp" "-b" "-"]
               stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t
               coprocess: (lambda (p) (write-string key p) (close-output-port p) (read-line p))))

(def (show-otp user (key (second (assoc user otp-keys))))
  (printf "~a  ~a\n" (generate-otp key) user))

(def (show-all-otps)
  (for-each (match <> ([user key] (show-otp user key))) otp-keys))

(define-entry-point (otp (user #f))
  (help: "show otp"
   getopt: [(optional-argument 'user)])
  (if user (show-otp user) (show-all-otps)))

(def (plist->keys plist)
  (map (match <> ([k . v] (list (as-string k) (as-string v))))
       (plist->alist plist)))

(def (register-otp-keys . plist)
  (set! otp-keys (append otp-keys (plist->keys plist))))
