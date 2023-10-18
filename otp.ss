;; OTP support using oathtool
#|
;; Install oathtool with a command such as:
apt install oathtool
nix-env -f '<nixpkgs>' -iA oath-toolkit

;; Create your personal executable otp script with content such as below:
#!/usr/bin/env gxi
(import :clan/otp)
(def main otp)
(set! otp-keys '(("myaccount@gmail.com" "abcd efgh ijkl mnop qrst uvwx yz12 3456")
                 ("myaccount@github.com" "0123456789ABCDEF")))
|#

(export #t)

(import
  (only-in :std/format printf)
  (only-in :std/getopt optional-argument)
  (only-in :std/misc/process run-process)
  (only-in :std/srfi/1 second)
  (only-in :clan/multicall define-entry-point))

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
