(export lazy-import-test)

(import :std/sugar :std/test
        ../lazy-import
        "lazy-import-test/box.ss")
(lazy-import
  ("lazy-import-test/module.ss" (something another)))

(def lazy-import-test
  (test-suite "test suite for clan/lazy-import"
    (test-case "test lazy-import module effects delayed until first function call"
      (try
        (check-equal? (unbox effect-box) 'nothing)
        (check-equal? (something 1) ['something 1])
        (check-equal? (unbox effect-box) 'module-run)
        (set-box! effect-box 'after-module)
        (check-equal? (another 2) ['another 2])
        (check-equal? (unbox effect-box) 'after-module)
        (finally (set-box! effect-box 'nothing))))))
