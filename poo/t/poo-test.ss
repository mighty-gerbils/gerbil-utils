(export
  poo-test)

;; NB: For debugging, use (import :std/interactive)

(import
  :gerbil/gambit/ports
  :std/format :std/sort :std/srfi/13 :std/test
  :clan/utils/assert :clan/utils/base
  ../poo ../brace)


(def poo-test
  (test-suite "test suite for clan/poo/poo"
    (test-case "simple tests from poo.md"
      (assert-equal! (poo? (.o (x 1) (y 2))) #t)
      (assert-equal! (poo? 42) #f)
      (.def foo (x 1))
      (.def! foo y (x) (+ x 3))
      (assert-equal! (.get foo y) 4)
      (.def bar (x 1))
      (assert-equal! (.get bar x) 1)
      (.set! bar x 18)
      (assert-equal! (.get bar x) 18)
      (assert-equal! (.key? foo 'y) #t)
      (assert-equal! (.has? foo z) #f)
      (def (sort-symbols symbols) (sort symbols (λ (a b) (string< (symbol->string a) (symbol->string b)))))
      (assert-equal! (sort-symbols (.all-slots foo)) '(x y))
      (assert-equal! (sort-symbols (.all-slots bar)) '(x))
      (def my-point (.o (x 3) (y 4)))
      (.def blued (color 'blue))
      (def my-colored-point (.mix blued my-point))
      (assert-equal! (.ref my-colored-point 'x) 3)
      (assert-equal! (.ref my-colored-point 'y) 4)
      (assert-equal! (.ref my-colored-point 'color) 'blue)
      (assert-equal! (.get my-colored-point x) (.ref my-colored-point 'x))
      (def complex (.o (:: @ [] x y) (x+iy (+ x (* 0+1i y)))))
      (.def (polar @ [] x+iy)
         (rho (magnitude x+iy)) (theta (angle x+iy)))
      (assert-equal! (.get (.mix my-colored-point polar complex) rho) 5)
    (test-case "slot tests from poo.md"
      (let ((x 1) (y 2))
        (.def point (x) (y))
        (assert-equal! (map (cut .ref point <>) '(x y)) [1 2]))
      (.def gerbil-config
        (modules => prepend '(gerbil gambit)))
      (def (prepend x y) (append y x))
      (.def base-config
        (modules '(kernel stdlib init)))
      (assert-equal! (.get (.mix gerbil-config base-config) modules)
                     '(gerbil gambit kernel stdlib init))
      (.def (hello @ [] name)
        (language 'en)
        (greeting (format greeting-fmt name))
        (greeting-fmt "hello, ~a"))
      (.def (localize-hello @ [hello] language)
        (name "poo")
        (greeting-fmt (next) (if (eq? language 'fr) "salut, ~a" (next))))
      (.def (french-hello @ localize-hello)
        (language 'fr))
      (assert-equal! (.get localize-hello greeting) "hello, poo")
      (assert-equal! (.get french-hello greeting) "salut, poo")))
    (test-case "simple hello tests"
      (.def hello
        (name (error "Undefined"))
        (greeting (format "Hello, ~a." name))
        (level 0))
      (.def (alice @ hello)
        (name "Alice")
        (level => + 1)
        (language 'english)
        (greeting (previous) (if (eq? language 'french) (format "Salut, ~a." name) (previous))))
      (.def (bob @ alice greeting)
        (name "Bob")
        (level => + 1)
        (greet (λ () (displayln greeting))))
      (def french (.o (language 'french) (level => + 1)))
      (assert-equal! (.get alice name) "Alice")
      (assert-equal! (.get bob name) "Bob")
      (assert-equal! (.get alice greeting) "Hello, Alice.")
      (assert-equal! (.get hello level) 0)
      (assert-equal! (.get alice level) 1)
      (assert-equal! (.get bob level) 2)
      (assert-equal! (.get (.mix french bob) level) 3)
      (assert-equal! (with-output-to-string (λ () (.call bob greet))) "Hello, Bob.\n")
      (assert-equal! (with-output-to-string (λ () (.call (.mix french bob) greet))) "Salut, Bob.\n"))
    (test-case "testing side-effects"
      (def foo (.o (x 6)))
      (.def! foo y (x) (* x 7))
      (.def (bar @ foo x y) (z (+ x 3)) (all [x y z]))
      (assert-equal! (.get foo x) 6)
      (assert-equal! (.get foo y) 42)
      (assert-equal! (.get bar x) 6)
      (assert-equal! (.get bar y) 42)
      (.set! bar x 1)
      (assert-equal! (.get foo x) 6)
      (assert-equal! (.get foo y) 42)
      (assert-equal! (.get foo x) 6)
      (assert-equal! (.get foo y) 42)
      (assert-equal! (.get bar x) 1))
    (test-case "keyword and brace syntax"
      (assert-equal! 2 (.get (.o a: 1 b: (+ a 1)) b))
      (assert-equal! 2 (.get {a: 1 b: (+ a 1)} b))
      (assert-equal! 2 (let ((a 0)) (.get (.o a: 1 b: (+ a 1)) b))) ;; proper shadowing
      (assert-equal! 2 (let ((a 0)) (.@ {a: 1 b: (+ a 1)} b))) ;; proper shadowing
    (test-case "referring to another method"
      (def m (.o a: 1+ b: a c: ((lambda (aa) (lambda (x) (aa x))) a) d: (lambda (x) (a x))))
      (assert-equal! (map (lambda (x) ((.ref m x) 2)) '(a b c d)) [3 3 3 3]))
    )))
