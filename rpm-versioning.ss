;;; Comparing

(export #t)
(import
  :std/assert :std/misc/list :std/pregexp :std/srfi/13 :std/sugar
  ./base ./basic-parsers ./order ./pred)

(def (valid-rpm-version-component? string start: (start 0) end: (end (string-length string)))
  (and
    (string-index string char-ascii-numeric? start end)
    (not (string-index string (cut string-index <> "-~/") start end))
    (not (string-contains string ".." start end))))

(def (valid-rpm-architecture-component? string start: (start 0) end: (end (string-length string)))
  (and
   (string-index string char-ascii-alphabetic? start end)
   (not (string-index string (complement char-ascii-alphanumeric-or-underscore?) start end))))

(def (parse-rpm-versioned-name string start: (start 0) end: (end (string-length string)))
  (def (err) (error "No valid RPM version in package name" string))
  (def (split-at n) (values (substring string start n)
                            (substring string (1+ n) end)))
  (def pos (string-index-right string #\- start end))
  (unless (and pos (valid-rpm-version-component? string start: (1+ pos) end: end))
    (err))
  (def pos2 (string-index-right string #\- start pos))
  (split-at (if (and pos2 (valid-rpm-version-component? string start: (1+ pos2) end: pos))
              pos2 pos)))

(def (rpm-versioned-name-basename string)
  (nth-value 0 (parse-rpm-versioned-name string)))

(def (rpm-versioned-name-version string)
  (nth-value 1 (parse-rpm-versioned-name string)))

(def (parse-rpm-path path)
  (def directory (path-directory path))
  (def extension (path-extension path))
  (def basename (path-strip-extension (path-strip-directory path)))
  (def dotpos (string-index-right basename #\.))
  (assert! (equal? extension "rpm"))
  (assert! dotpos)
  (assert! (valid-rpm-architecture-component? basename start: (1+ dotpos)))
  (def architecture (substring basename (1+ dotpos) (string-length basename)))
  (defvalues (name version) (parse-rpm-versioned-name basename end: dotpos))
  (values directory name version architecture))

(def (rpm-path-packagename path)
  (nth-value 1 (parse-rpm-path path)))

(def (rpm-pathname-version path)
  (nth-value 2 (parse-rpm-path path)))

;; For version comparison, I followed
;; https://twiki.cern.ch/twiki/bin/view/Main/RPMAndDebVersioning

;; Given a version or release component of a RPM, parse it into a list
;; of numbers and letters, e.g. "0.99p7" => (0 99 "p" 7)
(def (parse-rpm-version-component v)
  (def r [])
  (def len (string-length v))
  (def i 0)
  (def (handle-component predicate f)
    (when (and (< i len) (predicate (string-ref v i)))
      (let (j (or (string-index v (complement predicate) (1+ i)) len))
        (when f (push! (f (substring v i j)) r))
        (set! i j))))
  (while (< i len)
    (handle-component char-ascii-numeric? string->number)
    (handle-component char-ascii-alphabetic? identity)
    (handle-component (complement char-ascii-alphanumeric?) #f))
  (reverse r))

;; Given the first chunks of two respective version numbers,
;; return the symbol < = > depending on which of predicates hold,
;; or #f is none does
(def (compare-rpm-version-chunks ch1 ch2)
  (assert! (or (integer? ch1) (string? ch1)))
  (assert! (or (integer? ch2) (string? ch2)))
  (cond
    ((and (integer? ch1) (integer? ch2))
     (cond
       ((< ch1 ch2) '<)
       ((> ch1 ch2) '>)
       (else '=)))
    ;; RPM: integer block beats alphanumeric, so 1.4.1 > 1.4p8
    ((integer? ch1) '>)
    ((integer? ch2) '<)
    (else
     (cond
       ((string<? ch1 ch2) '<)
       ((string>? ch1 ch2) '>)
       (else '=)))))

(def (compare-rpm-version-components v1 v2)
  (def l1 (parse-rpm-version-component v1))
  (def l2 (parse-rpm-version-component v2))
  (let/cc return
    (until (or (null? l1) (null? l2))
      (let (r (compare-rpm-version-chunks (pop! l1) (pop! l2)))
        (case r
          ((< > #f) (return r))
          ((=) #f))))
    (cond
      ((null? l1) (return '<))
      ((null? l2) (return '>))
      (else (return '=)))))

(def (parse-rpm-version x)
  (match (pregexp-match "^(?:([0-9]+):)?([^-/~]+)(?:-([^-/~]+))?$" x) ;; TODO: also forbid ..
    ([_ epoch version release]
     (values (if (string-empty? epoch) 0 (string->number epoch))
             version release))
    (error "bad rpm version" x)))

(def (compare-rpm-versions v1 v2)
  (defvalues (epoch1 version1 release1) (parse-rpm-version v1))
  (defvalues (epoch2 version2 release2) (parse-rpm-version v2))
  (cond
   ((> epoch1 epoch2) '>)
   ((< epoch1 epoch2) '<)
   (else
    (let (r (compare-rpm-version-components version1 version2))
      (case r ((< > #f) r)
            ((=) (compare-rpm-version-components release1 release2)))))))

(def (rpm-version<= v1 v2)
  (case (compare-rpm-versions v1 v2)
    ((< =) #t)
    ((>) #f)))

(def (rpm-version>= v1 v2)
  (case (compare-rpm-versions v1 v2)
    ((> =) #t)
    ((<) #f)))

(def (rpm-version< v1 v2)
  (case (compare-rpm-versions v1 v2)
    ((<) #t)
    ((> =) #f)))

(def (rpm-version> v1 v2)
  (case (compare-rpm-versions v1 v2)
    ((>) #t)
    ((< =) #f)))

(def (rpm-version= v1 v2)
  (case (compare-rpm-versions v1 v2)
    ((=) #t)
    ((< >) #f)))

