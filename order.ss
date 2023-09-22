(export #t)

(import :gerbil/gambit)

;; Lexicographically compare two lists of using the function element<? to compare elements.
;; element<? is a strict total order; the resulting order on X and Y will also be strict."
(def (lexicographic<? element<? x y)
  (cond ((null? y) #f)
        ((null? x) #t)
        ((element<? (car x) (car y)) #t)
        ((element<? (car y) (car x)) #f)
        (else (lexicographic<? element<? (cdr x) (cdr y)))))

;; Lexicographically compare two lists of using the function element< to compare elements.
;; element< is a strict total order; the resulting order on X and Y will be a non-strict total order.
(def (lexicographic<=? element<? x y)
  (not (lexicographic<? element<? y x)))



(def (parse-dotted-version s)
  (map string->number (string-split s #\.)))
(def (unparse-dotted-version s)
  (string-join (map object->string s) "."))

;; Parse the version as a string, and return the next version as a string
(def (next-version version)
  (match (reverse (parse-dotted-version version))
    ([last . rprefix] (unparse-dotted-version (reverse (cons (1+ last) rprefix))))))

(def (dotted-version<? version1 version2)
  (lexicographic<? < (parse-dotted-version version1) (parse-dotted-version version2)))
(def (dotted-version<=? version1 version2)
  (not (dotted-version<? version2 version1)))
(def (dotted-version>? version1 version2)
  (dotted-version<? version2 version1))
(def (dotted-version>=? version1 version2)
  (dotted-version<=? version2 version1))
