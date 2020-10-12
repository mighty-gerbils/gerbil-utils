;; -*- Gerbil -*-
;;;; Utilities to generate diceware phrases
;; Usage: Have DICEWARE_FILE point to the path of your favorite diceware word list:
;; https://theworld.com/~reinhold/diceware.html
;; https://github.com/ulif/diceware/blob/master/docs/wordlists.rst
;; https://github.com/bitcoin/bips/blob/master/bip-0039/bip-0039-wordlists.md

(export #t)

(import
  :gerbil/gambit/random
  :std/format :std/iter :std/misc/list :std/misc/ports :std/pregexp :std/srfi/13 :std/sugar
  ./base ./basic-parsers ./basic-printers ./basic-parsers ./number ./random)

(def diceware-file (getenv "DICEWARE_FILE" #f))

(def *diceware-words* #f)

(def (get-diceware-words)
  (unless *diceware-words*
    (set! *diceware-words* (parse-diceware-file diceware-file)))
  *diceware-words*)

;; Add an offset to a char's encoding, return a new char.
;; Return #f if the new index is out of bounds.
;; : (Or Char '#f) <- Char
(def (char+ char offset)
  (let ((index (+ (char->integer char) offset)))
    (and (< -1 index #x110000)
         (integer->char index))))

;; Given a roll of dice as string of dice numbers from 1 to 6,
;; return the index for an entry in a diceware dictionary.
;; : Integer <- String
(def (diceware-index<-string string)
  (call-with-input-string (string-map (cut char+ <> -1) string) (cut expect-natural <> 6)))

;; Given an index in a diceware dictionary and the number of dice for the dictionary,
;; return the index for the entry as a string of dice numbers from 1 to 6.
;; : String <- Integer Integer
(def (string<-diceware-index index n-dice)
  ;; (assert! (< -1 index (expt 6 n-dice)))
  (!> index
      (cut + <> (expt 6 n-dice)) ;; add a 1 in front to ensure leading 0s
      (cut string<-integer-base <> 6) ;; convert to base 6
      (cut string-drop <> 1) ;; drop the 1 in front from two steps ago
      (cut string-map (cut char+ <> +1) <>))) ;; add 1 to each digit

;; Robustly read a diceware file as input
;; 1- the list of words is sorted asciibetically
;; 2- the list of indices increases from 0
;; 3- indices all have the same length
;; 4- the total number of entries is (expt 6 (string-length index))
(def (parse-diceware-file file)
  (assert! file)
  (nest
   (call-with-input-file [path: file]) (λ (port))
   (match (pregexp-match "^([1-6]+) ([!-~]+)$" (read-line port)))
   ([_ first-index first-word])
   (let* ((n-dice (string-length first-index))
          (v (make-vector (expt 6 n-dice) first-word))))
   (let loop ((i 1)
              (previous-word first-word)))
   (let ((line (read-line port))
         (j (+ i 1))))
   (if (eof-object? line)
     (begin
       (assert! (= i (expt 6 n-dice)))
       v))
   (match (pregexp-match "^([1-6]+) ([!-~]+)$" line))
   ([_ index word]
    (assert! (= (string-length index) n-dice))
    (unless (= i (diceware-index<-string index)) (error "foo" i index word))
    (assert! (= i (diceware-index<-string index)))
    (assert! (string< previous-word word))
    (vector-set! v i word))
   (loop j word)))

(def (dump-diceware-file path-and-settings words)
  (let ((n-dice (integer-part (/ (log (vector-length words)) (log 6)))))
    (assert! (= (vector-length words) (expt 6 n-dice)))
    (call-with-output-file
        path-and-settings
      (λ (port)
        (for (i (in-iota (vector-length words)))
           (fprintf port "~a ~a\n" (string<-diceware-index i n-dice) (vector-ref words i)))))))

(def (diceware-word (index #f))
  (let* ((words (get-diceware-words))
         (len (vector-length words))
         (i (or index (random-integer len))))
    (vector-ref words i)))

;; 20 words of 5 dice is just over 258 bits. Half of that is probably more than needed.
(def (diceware-phrase (n-words 20))
  (string-join (with-list-builder (c _) (for (_ (in-iota n-words)) (c (diceware-word)))) " "))
