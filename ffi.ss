;; -*- Gerbil -*-
;;;; FFI to standard C libraries
;; TODO:
;; * Make it a complete set of trivial wrappers around libc functions.
;;   Automate import or at least steal the wrappers from another language implementation.
;; * Make it portable to Linux, Windows, macOS, FreeBSD, OpenBSD.
;;   define and register stubs for functions not present.
;; * Write tests
;;
;;; XXX vyzo: kill/getpid are candidates for stdlib integration; perhaps std/os/process
(export #t)

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports
  :std/misc/process :std/sugar
  ./base)

(begin-foreign
  (c-declare #<<END-C
#define __USE_GNU 1
#include <unistd.h>
#include <signal.h>
#include <errno.h>
END-C
  )

  (define-macro (define-c-lambda id args ret #!optional (name #f))
    (let ((name (or name (##symbol->string id))))
      `(define ,id
         (c-lambda ,args ,ret ,name))))

  (define-macro (define-with-errno symbol ffi-symbol args)
    `(define (,symbol ,@args)
       (declare (not interrupts-enabled))
       (let ((r (,ffi-symbol ,@args)))
         (if (##fx< r 0)
           (##fx- (__errno))
           r))))

  (define-c-lambda __errno () int
    "___return (errno);")

  (namespace ("clan/ffi#"
              __kill _kill getpid
              ))

  (define-c-lambda __kill (int int) int
    "kill")
  (define-with-errno _kill __kill (pid sig))

  (define-c-lambda getpid () int
    "getpid")
  )
(extern _kill getpid)

(def (kill pid (sig SIGTERM)) (_kill pid sig))

;; Portable POSIX signal numbers according to https://en.wikipedia.org/wiki/Signal_(IPC)
(def SIGHUP 1)
(def SIGINT 2)
(def SIGQUIT 3)
(def SIGTRAP 5)
(def SIGABRT 6)
(def SIGKILL 9)
(def SIGALRM 14)
(def SIGTERM 15)
