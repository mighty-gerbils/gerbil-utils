;; -*- Gerbil -*-
;;;; Date utilities

;; TODO: rename this file to timestamp.ss

(export #t)

(import
  :gerbil/gambit/exact :gerbil/gambit/threads
  :scheme/base
  :std/format :std/srfi/19 :std/sugar
  :utils/base :utils/basic-parsers :utils/number)

;; Internally, our "timestamp" datatype represent time as a bignum integer counting nanoseconds
;; of "official time" (ignoring leap seconds) since the Unix epoch of 1970-01-01T00:00Z.
;; This is one billion time the unix time as returned by e.g. (shell-command "date +%s")
;;
;; Issue 0: The largest fixnum on Gambit 64-bit being 2**61-1, this representation is efficient
;; to year 2073; it will be efficient as well when used for interchange with C and other languages
;; using 64-bit signed or unsigned integers.
;;
;; Issue 1: Because this format notably jumps over leap seconds, it is therefore not monotonic.
;; It is unclear what happens when you use 'time-monotonic instead of the default 'time-utc,
;; notably with respect to leap seconds; but that's what needs to be used for timers.
;;
;; Issue 2: A timestamp will NOT fit in a double, and precision will underflow below microseconds
;; or a fraction thereof. This is can be a slight problem when serializing to plain JSON.

;; Units of duration. Use these variables to abstract over our choice of base duration.
(def one-nanosecond 1)
(def one-microsecond 1000)
(def one-millisecond (* 1000 1000))
(def one-second (* 1000 1000 1000))
(def one-minute (* 60 one-second))
(def one-hour (* 60 one-minute))
(def one-day (* 24 one-hour))
(def three-days (* 3 one-day)) ;; notably used below
(def one-week (* 7 one-day))
(def fifteen-days (* 15 one-day))


;;; Conversion utilities using SRFI 19.

;; Get a SRFI 19 date object from a string in the YYYYMMDD format (Yay Y10K problem!)
;; NB: We always use UTC time in data structures.
;; Also all our servers use UTC as their default system "timezone".
(def (date<-yyyymmdd yyyymmdd)
  (string->date yyyymmdd "~Y~m~d"))

;; Date object for Unix Epoch
(def unix-epoch-date (date<-yyyymmdd "19700101"))

;; Offset in second for Unix Epoch
(def unix-epoch-offset (time-second (date->time-utc unix-epoch-date)))

;; Create a SRFI 19 time-utc structure from a unix-time which is an integer since the epoch
(def (time-utc<-unix-time unix-time)
  (cond
   ((exact-integer? unix-time)
    (make-time time-utc 0 (+ unix-time unix-epoch-offset)))
   ((real? unix-time)
    (let* ((real-time (+ unix-time unix-epoch-offset))
           (sec (integer-part real-time))
           (nanosec (integer-part (* 1e9 (- real-time sec)))))
      (make-time time-utc nanosec sec)))
   (else (error "invalid unix-time" unix-time))))

;; Get Unix time from a SRFI 19 time-utc structure
(def (unix-time<-time-utc utc)
  (let* ((sec (- (time-second utc) unix-epoch-offset))
         (nanosec (time-nanosecond utc)))
    (if (zero? nanosec)
      sec
      (+ sec (* nanosec 1e-9)))))

;; Get Unix time from a SRFI 19 date structure
(def (unix-time<-date date)
  (unix-time<-time-utc (date->time-utc date)))

;; Get Unix time from a YYYYMMDD string
(def (unix-time<-yyyymmdd yyyymmdd)
  (unix-time<-date (date<-yyyymmdd yyyymmdd)))

;; Get a SRFI 19 date structure from a Unix time
(def (date<-unix-time u)
  (time-utc->date (time-utc<-unix-time u)))

;; Get a string from a Unix time. The format defaults to ISO 8601.
(def (string<-unix-time u (format-string "~4"))
  (date->string (date<-unix-time u) format-string))

;; Get a Unix time from string. The format defaults to ISO 8601.
(def (unix-time<-string i (format-string "~Y-~m-~dT~H:~M:~SZ"))
  (unix-time<-date (string->date i format-string)))

;; Given a day, return the start of the day
(def (date-start-of-day date)
  (make-date 0 0 0 0 (date-day date) (date-month date) (date-year date) (date-zone-offset date)))

(def (time-utc-start-of-day time-utc)
  (date->time-utc (date-start-of-day (time-utc->date time-utc))))

(def (unix-time-start-of-day unix-time)
  (unix-time<-time-utc (time-utc-start-of-day (time-utc<-unix-time unix-time))))


;; Given a SRFI 19 time structure, return a bignum of nanoseconds since epoch.
(def (timestamp<-srfi-19-time time)
  (+ (time-nanosecond time)
     (* one-second (time-second time))))

;; Given a SRFI 19 time structure, return a bignum of nanoseconds since epoch.
(def (srfi-19-time<-timestamp timestamp (type time-utc))
  (make-time type
             (remainder timestamp one-second)
             (quotient timestamp one-second)))

;;; Return the current timestamp
(def (current-timestamp (time-type 'time-utc))
  (timestamp<-srfi-19-time (current-time time-type)))

;;; Return the current monotonic timestamp
(def (current-monotonic-timestamp)
  (current-timestamp 'time-monotonic))

;;; Convert a timestamp to a SRFI 19 date object
(def (date<-timestamp timestamp)
  (time-utc->date (srfi-19-time<-timestamp timestamp)))

;;; Convert a SRFI 19 date object to a timestamp
(def (timestamp<-date date)
  (timestamp<-srfi-19-time (date->time-utc date)))

;;; A string for the date represented by this timestamp.
;;; The format defaults to ISO 8601 format with nanoseconds.
(def (string<-timestamp timestamp (format-string "~Y-~m-~dT~k:~M:~S.~N~z"))
  (date->string (date<-timestamp timestamp) format-string))

;;; Parse a timestamp as per SRFI 19.
;;; The format defaults to ISO 8601 format with nanoseconds.
(def (timestamp<-string string (format-string "~Y-~m-~dT~k:~M:~S.~N~z"))
  (timestamp<-srfi-19-time (date->time-utc (string->date string format-string))))

;;; Get start of day for a given timestamp
(def (timestamp<-unix-time unix-time)
  (* unix-time one-second))

;;; Get start of day for a given timestamp
(def (timestamp-start-of-day timestamp)
  (period-start one-day timestamp))

;; Get the timestamp from a YYYYMMDD string
(def (timestamp<-yyyymmdd yyyymmdd)
  (timestamp<-string yyyymmdd "~Y~m~d"))

;;; Display and parse a timestamp
(def display-timestamp display)
(def expect-timestamp expect-natural)

;; Minimum meaningful sleep quantum, in seconds. Matches the precision of our clock.
(def +sleep-quantum-in-nanoseconds+ 1000)

(def (sleep nanoseconds)
  (thread-sleep! (* 1e-9 nanoseconds)))

(def min-timestamp 0)
(def max-timestamp (- (expt 2 63) 1)) ;; NB: Bug in year 2292!
(def srfi-19-time-quantum (make-time 'time-duration 0 1)) ;; a SRFI-19 object


(def (date-string<-timestamp timestamp)
  (string<-timestamp timestamp "~Y-~m-~d"))

;; Like date-string<-timestamp but caching the previous answer.
;; NB: closures created by this function are not thread-safe.
(def (caching-date-string<-timestamp)
  (let ((previous-date-start 0)
        (previous-date-end (- one-day 1)) ;; we use <= and assume integers.
        (date-string "1970-01-01"))
    (λ (timestamp)
      (unless (<= previous-date-start timestamp previous-date-end)
        (set! previous-date-start (timestamp-start-of-day timestamp))
        (set! previous-date-end (+ previous-date-start one-day -1))
        (set! date-string (date-string<-timestamp previous-date-start)))
      date-string)))

;; Periods.
;; Return the start of a period specified by periodicity, random timestamp in the period, and
;; number of additional periods to add (or remove if negative) to the otherwise specified period.
;; Periodicity can be a number (in nanoseconds), or a symbol: week, month, year
;; Periodicity := (Or Nat 'month 'year)
;; Periods assume "official seconds" that skip leap second.
;; Each period is an interval that start at the period-start (included) and
;; ends at the period-start of the next interval (with additional-periods +1).
;; NB: our time intervals include the start exclude the end.
;; Timestamp <- Periodicity Timestamp Integer
(def (period-start periodicity timestamp (additional-periods 0))
  (match periodicity
    ((? exact-integer?) (* (+ (floor-quotient timestamp periodicity) additional-periods) periodicity))
    ;; Our weeks start on Monday. The Epoch was a Thursday.
    ('week (- (* (+ (floor-quotient (+ timestamp three-days) one-week) additional-periods) one-week) three-days))
    ('month (let* ((date (date<-timestamp timestamp))
                   (months (+ (date-month date) (* 12 (date-year date)) additional-periods -1))
                   (month (+ (modulo months 12) 1))
                   (year (floor-quotient months 12)))
              (timestamp<-date (make-date 0 0 0 0 1 month year 0))))
    ('year (let ((date (date<-timestamp timestamp)))
             (timestamp<-date (make-date 0 0 0 0 1 1 (+ (date-year date) additional-periods) 0))))
    ((? object?) (period-start {periodicity periodicity} timestamp additional-periods))))

;; The next period, which is also the end of the current period.
(def (period-next periodicity timestamp)
  (period-start periodicity timestamp +1))

;; Count the number of periods of given periodicity from one period to the other,
;; each designated by the timestamp at the start of the period (NB: may be off if not the case)
;; i.e. for integer periodicity, (timestamp2-timestamp1)/periodicity
(def (period-difference periodicity timestamp1 timestamp2)
  (match periodicity
    ((? exact-integer?) (floor-quotient (- timestamp2 timestamp1) periodicity))
    ('week (floor-quotient (- timestamp2 timestamp1) one-week))
    ('month (let* ((date1 (date<-timestamp timestamp1))
                   (months1 (+ (date-month date1) (* 12 (date-year date1))))
                   (date2 (date<-timestamp timestamp2))
                   (months2 (+ (date-month date2) (* 12 (date-year date2)))))
              (- months2 months1)))
    ('year (- (date-year (date<-timestamp timestamp2)) (date-year (date<-timestamp timestamp1))))
    ((? object?) (period-difference {periodicity periodicity} timestamp1 timestamp2))))

;; Similar to period-start, but with ceiling instead of floor
(def (period-after periodicity timestamp)
  (let ((start (period-start periodicity timestamp)))
    (if (= start timestamp) start (period-next periodicity start))))

(def (periodicity? x)
  (or (and (exact-integer? x) (< 0 x))
      (member x '(week month year))))

(def (sleep-until target-timestamp)
  (let* ((now (current-timestamp))
         (duration (- target-timestamp now)))
    (when (positive? duration)
      (sleep duration))))

;; This function keeps indefinitely calling a thunk on a periodic basis.
;; The function will be called at every "beat" defined by the timestamp being divisible by the period.
;; If running the thunk takes more time than the period, then on-beat-skip is called
;; and the next call is scheduled for the next beat after on-beat-skip returns.
(def (periodically period-in-nanoseconds thunk on-beat-skip: (on-beat-skip void))
  (let ((target-timestamp (ceiling-align (current-timestamp) period-in-nanoseconds)))
    (while #t
      (thunk)
      (increment! target-timestamp period-in-nanoseconds)
      (when (> (current-timestamp) target-timestamp)
        (on-beat-skip)
        (set! target-timestamp (ceiling-align (current-timestamp) period-in-nanoseconds)))
      (sleep-until target-timestamp))))

