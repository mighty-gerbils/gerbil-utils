;; OTP support using oathtool
#|
;; Install oathtool with a command such as:
apt install oathtool
nix-env -f '<nixpkgs>' -iA oath-toolkit

;; Create your personal executable otp script with content such as below:
#!/usr/bin/env gxi
(export #t)
(import :clan/otp)
(define-multicall-main)
(set-default-entry-point! 'otp)
(register-otp-keys
  myaccount@gmail.com: "abcd efgh ijkl mnop qrst uvwx yz12 3456"
  myaccount@github.com: "0123456789ABCDEF")
|#

(export #t)

(import
  (only-in :std/cli/getopt argument optional-argument)
  (only-in :std/cli/multicall define-entry-point)
  (only-in :std/format format printf eprintf)
  (only-in :std/misc/list group-n-consecutive)
  (only-in :std/misc/alist plist->alist)
  (only-in :std/misc/process run-process run-process/batch)
  (only-in :std/net/uri uri-encode)
  (only-in :std/srfi/1 second)
  (only-in :std/srfi/13 string-prefix?)
  (only-in :std/sugar if-let)
  (only-in ./temporary-files call-with-temporary-file))

(def otp-keys (values []))

(def (generate-otp key)
  (run-process ["oathtool" "--totp" "-b" "-"]
               stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t
               coprocess: (lambda (p) (write-string key p) (close-output-port p) (read-line p))))

(def (key-finder issuer (user #f))
  (if user
    (lambda (e) (match e ([i u _] (and (equal? issuer i) (equal? user u) e)) (else #f)))
    (lambda (e) (match e ([i _ _] (and (equal? issuer i) e)) (else #f)))))

(def (find-key issuer user (keys otp-keys))
  (find (key-finder issuer user) keys))

(def (show-otp issuer user (key #f))
  (if key (printf "~a ~a: ~a\n" (generate-otp key) issuer user)
      (if-let (e (find-key issuer user))
        (show-otp issuer (second e) (third e))
        (begin
          (eprintf "Key not found\n")
          (values)))))

(def (show-all-otps)
  (for-each (match <> ([issuer user key] (show-otp issuer user key))) otp-keys))

(define-entry-point (otp (issuer #f) (user #f))
  (help: "show otp"
   getopt: [(optional-argument 'issuer)
            (optional-argument 'user)])
  (if issuer (show-otp issuer user) (show-all-otps)))

(def (register-otp-keys . l)
  (set! otp-keys (append otp-keys (map (cut map as-string <>) (group-n-consecutive 3 l)))))

(define-entry-point (otpauth issuer (user #f))
  (help: "show otpauth url"
   getopt: [(argument 'issuer)
            (optional-argument 'user)])
  (def entry (find-key issuer user))
  (match entry
    ([i u k] (format "otpauth://totp/~a:~a?secret=~a" (uri-encode i) (uri-encode u) (uri-encode k)))
    (else (error "key not found"))))

(define-entry-point (qr issuer (user #f))
  (help: "show otpauth qr"
   getopt: [(argument 'issuer)
            (optional-argument 'user)])
  (def uri (otpauth issuer user))
  (printf "~a\n" uri)
  (def view "feh --auto-zoom --fullscreen --draw-actions --draw-filename --draw-tinted")
  (call-with-temporary-file
   prefix: "tmp.qr."
   suffix: ".png"
   after-close: (lambda (png)
                  (run-process/batch ["/bin/sh" "-c"
                                      (format "qrencode -s 5 -o ~a ~a && ~a ~a" png uri view png)]))))
