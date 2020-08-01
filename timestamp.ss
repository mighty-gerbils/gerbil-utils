;; -*- Gerbil -*-
;;;; Timestamp utilities
;; Renamed from date.ss on 2020-04-26

(export #t)

(import
  :gerbil/gambit/exact :gerbil/gambit/threads
  :scheme/base
  :std/format :std/misc/number :std/srfi/19 :std/sugar
  ./base ./basic-parsers ./number)

;; We deal with several time representations:
;;
;;    1. unix-time, which is in "official" seconds since the Unix Epoch 1970-01-01T00:00Z, wherein
;;       each legal day is divided in 86400 POSIX seconds, regardless of its actual duration,
;;       as measured in either atomic seconds or number of seconds in the UTC day
;;       (which will be 86399 or 86401, for a negative or positive leap second respectively).
;;       This is time that Unix systems (Linux, Darwin, *BSD) traditionally use throughout,
;;       also known as POSIX time, after the standardization body that defines it.
;;       The shell command $(date +%s) returns it.
;;       The system call clock_gettime(3) with CLOCK_REALTIME provides this time in a timespec
;;       data structure that also includes a separate nanosecond counter
;;       (which hails back to the era of 32-bit computing).
;;       The SRFI-19 time structure that mimic it, with time-utc type, also uses it.
;;       This is also the time maintained by NTP. However, there is a big issue with unix-time.
;;       *Depending on your system is configured*, unix-time may either:
;;         (a) repeat during a UTC positive leap second (which has happened 27 times as of 2020)
;;           and skip ahead during a UTC negative leap second (which hasn't happened so far),
;;           which is traditional POSIX-specified behavior https://en.wikipedia.org/wiki/Unix_time
;;           but ambiguous (and redundant, at nanosecond scale) during a positive leap second, or
;;         (b) "smear" time around a leap second, commonly with a linear 1/86400 rate change, from
;;           noon UTC the day before to noon UTC the day after, which is what NTP servers from
;;           Google, Amazon, and others, do. https://developers.google.com/time/smear
;;       This second configuration yields much more useful results and is now well-supported, but
;;       is not guaranteed behavior, so be careful about OS configuration before you depend on it.
;;       The first configuration is wholly useless to measure small time intervals.
;;       The second configuration may introduce 1/86400 discrepancy ratio in time intervals
;;       unless first converted to TAI using a simple formula.
;;       Conversion between unix-time and tai requires an up-to-date leap second database,
;;       but that database may change every six months, and that requires system administration
;;       to keep it up-to-date for on-disk configuration, but also in-process configuration.
;;
;;    2. unix-timestamp, which is our variant of unix-time, scaled to a nanosecond precision,
;;       combining the two fields in a single fixnum, which on modern 64-bit machines is
;;       simpler and cheaper for all kinds of manipulations, and even with extended range
;;       (with a Y2043 issue, rather than Y2038 like 31-bit unix-time or Y2036 like NTP).
;;       There again, in a "leap" configuration, this variant may yield "surprising" results
;;       during a leap second, with non-monotonicity and repeat values.
;;       In a "smear" configuration, this is no loss of monotonicity and small loss of precision
;;       but there are still subtle rules to follow in some cases.
;;       Be careful about system administration if you use this representation.
;;
;;    3. tai-time, which is seconds since an epoch and *has no leap seconds*, such that
;;       a given timestamp always represents a duration interval of exactly one second.
;;       Some Linux servers are configured to use TAI, but most use unix-time above.
;;       The Epoch is such that tai at 1972-01-01T00:00Z is +63072010, or 10s more than
;;       unix-time (+63072000). Every 6 months, the IERS may or may not schedule a new leap second.
;;       The TAI - UTC adjustment gets updated accordingly (as of 2020Q2, TAI is unix-time + 37s),
;;       which is tracked by ICANN's tz database https://en.wikipedia.org/wiki/Tz_database
;;
;;    4. tai-timestamp, which is in our variant of tai, scaled to a nanosecond precision.
;;       The timestamp-tai representation is defined in this module and so far as I know
;;       original to this library (though probably independently reinvented).
;;
;;    5. SRFI-19 time, which is a struct with fields type, nanoseconds and seconds,
;;       where type is one of (time-{utc,tai,monotonic,thread,process,duration}).
;;       Modulo a slightly different representation, it tracks the same at the above four.
;;
;;    4. SRFI-19 date, which is a struct with fields
;;       nanosecond second minute hour day month year zone-offset,
;;       which is useful for decoding time between internal representation above,
;;       and human-readable strings.
;;
;; Issue 0: The largest fixnum on Gambit 64-bit being 2**61-1, so our timestamp representation
;; is efficient to year 2043 only (whereas 31-bit C unix-time expires year 2038).
;; Our representation will be efficient as well when used for interchange with
;; C and other languages using 64-bit signed or unsigned integers. As the fated year approaches,
;; and unless Gambit adds one bit to its fixnums, a change in the epoch might be useful,
;; which makes it all the more important to persist the perceived difference between timestamps
;; divided by a billion and unix-time together with timestamped data (see issues 1 and 3 below).
;;
;; Issue 1: Unix tends to keep its time in UTC, which isn't monotonic, and it's not clear what
;; happens to (current-unix-time). Or when there are NTP adjustments, when a laptop sleeps, etc.
;; The representation we chose should be future-proof, to the implementation may be buggy, and
;; even the representation is not past-proof to before 1972-01-01, and not future-proof past
;; whatever update to the table of leap seconds the software misses between the time it is built
;; or started, and the time it's running, when long-running, that gets a future timestamp.
;;
;; Issue 2: A timestamp will NOT in general fit in a 53-bit mantissa IEEE double
;; and precision will underflow below microseconds or a fraction thereof.
;; This is can be a slight problem when serializing to plain JSON, but also when using
;; Gambit's ##current-time's time data structure, which is notably different from SRFI-19 time.
;;
;; Issue 3: Whenever you persist data in tai-timestamp format, but also in unix-timestamp if
;; you use leap configuration, you may also want to save the TAI-UTC adjustment you used,
;; especially if for long-running programs: new leap seconds may be decreed or not every 6 months,
;; and your program may not have been updated, so may be using the wrong offset,
;; which will make interpretation of old logs and data hard.
;; NB: Unix systems as well as NTP seem to insist on keeping time in UTC, yet no time-keeping API
;; will tell you what they believe is the TAI adjustment or whether they are using smear.
;; You have to do your best, and pray when there's a leap second.
;; To estimate the TAI-UTC adjustment, pray your program was re-compiled and restarted recently,
;; or is somehow updated once in a while by querying an online server.
;; Your best seems to be to convert a given time between UTC and TAI, remember what the offset was,
;; and store whichever makes more sense.
;;
;; https://en.wikipedia.org/wiki/Leap_second
;; https://en.wikipedia.org/wiki/Leap_year <-- also an issue, but on much larger timescales,
;;   that programmers don't care about because their software is so brittle anyway.
;;
;; https://erlang.org/doc/apps/erts/time_correction.html

;;; Units of duration. Use these variables to abstract over our choice of base duration.
(def one-nanosecond 1)
(def one-microsecond 1000)
(def one-millisecond (* 1000 1000))
(def one-second (* 1000 1000 1000))
(def one-minute (* 60 one-second))
(def one-hour (* 60 one-minute))
(def one-day (* 24 one-hour))
(def three-days (* 3 one-day)) ;; notably used for day-of-week conversion and weekly periodicity
(def one-week (* 7 one-day))
(def fifteen-days (* 15 one-day))


;;; Normalization utilities for SRFI 19 time, used to get current timestamp.
(def (ensure-srfi-19-time-tai srfi-19-time)
  (case (time-type srfi-19-time)
    ((time-tai) srfi-19-time)
    ((time-utc) (time-utc->time-tai srfi-19-time))
    ((time-monotonic) (time-monotonic->time-tai srfi-19-time))
    (else (error "Time cannot be converted to TAI" srfi-19-time))))

(def (ensure-srfi-19-time-utc srfi-19-time)
  (case (time-type srfi-19-time)
    ((time-utc) srfi-19-time)
    ((time-tai) (time-tai->time-utc srfi-19-time))
    ((time-monotonic) (time-monotonic->time-utc srfi-19-time))
    (else (error "Time cannot be converted to UTC" srfi-19-time))))

(def (ensure-srfi-19-time-monotonic srfi-19-time)
  (case (time-type srfi-19-time)
    ((time-monotonic) srfi-19-time)
    ((time-utc) (time-utc->time-monotonic srfi-19-time))
    ((time-tai) (time-tai->time-monotonic srfi-19-time))
    (else (error "Time cannot be converted to monotonic" srfi-19-time))))

(def (ensure-srfi-19-time-type srfi-19-time type)
  (case type
    ((time-utc) (ensure-srfi-19-time-utc srfi-19-time))
    ((time-tai) (ensure-srfi-19-time-tai srfi-19-time))
    ((time-monotonic) (ensure-srfi-19-time-monotonic srfi-19-time))
    (else (error "Time cannot be converted to type" srfi-19-time type))))

;;; Now, basic timestamp definitions

(def (unix-timestamp<-srfi-19-time srfi-19-time)
  (unless (member (time-type srfi-19-time) '(time-utc time-duration))
    (set! srfi-19-time (ensure-srfi-19-time-utc srfi-19-time)))
  (+ (time-nanosecond srfi-19-time)
     (* one-second (time-second srfi-19-time))))

(def (tai-timestamp<-srfi-19-time srfi-19-time)
  (unless (member (time-type srfi-19-time) '(time-tai time-duration))
    (set! srfi-19-time (ensure-srfi-19-time-tai srfi-19-time)))
  (+ (time-nanosecond srfi-19-time)
     (* one-second (time-second srfi-19-time))))

;; Given a timestamp, a SRFI 19 time type (default time-tai), and an adjustment in seconds
;; (default #f, denoting the current implementation's belief of what TAI-UTC was at that timestamp),
;; return a SRFI 19 time structure with given type corresponding to the time denoted by timestamp,
;; which was stored by a UTC-synchronized system believing the adjustment was that of TAI-UTC.
(def (srfi-19-time<-tai-timestamp timestamp (type time-tai) (adjustment #f))
  (if (and adjustment (eq? type time-utc))
    (ensure-srfi-19-time-type (make-time time-utc
                                 (remainder timestamp one-second)
                                 (- (quotient timestamp one-second) adjustment))
                      type)
    (let ((srfi-19-time (make-time time-tai
                                   (remainder timestamp one-second)
                                   (quotient timestamp one-second))))
      (case type
        ((time-tai) srfi-19-time)
        ((time-duration) (set! (time-type srfi-19-time) time-duration) srfi-19-time)
        (else (ensure-srfi-19-time-type srfi-19-time type))))))

;; Given a timestamp, a SRFI 19 time type (default time-utc), and an adjustment in seconds
;; (default #f, denoting the current implementation's belief of what TAI-UTC was at that timestamp),
;; return a SRFI 19 time structure with given type corresponding to the time denoted by timestamp,
;; which was stored by a UTC-synchronized system believing the adjustment was that of TAI-UTC.
(def (srfi-19-time<-unix-timestamp timestamp (type time-utc) (adjustment #f))
  (if (and adjustment (eq? type time-tai))
    (ensure-srfi-19-time-type (make-time time-tai
                                 (remainder timestamp one-second)
                                 (+ (quotient timestamp one-second) adjustment))
                      type)
    (let ((srfi-19-time (make-time time-utc
                           (remainder timestamp one-second)
                           (quotient timestamp one-second))))
      (case type
        ((time-utc) srfi-19-time)
        ((time-duration) (set! (time-type srfi-19-time) time-duration) srfi-19-time)
        (else (ensure-srfi-19-time-type srfi-19-time type))))))

;; Return the current timestamp
(def (current-tai-timestamp)
  (tai-timestamp<-srfi-19-time (current-time time-tai)))

(def (current-unix-timestamp)
  (unix-timestamp<-srfi-19-time (current-time time-utc)))

;; Convert a timestamp to a SRFI 19 date object
(def (date<-tai-timestamp timestamp (tz-offset 0))
  (time-tai->date (srfi-19-time<-tai-timestamp timestamp time-tai) tz-offset))

(def (date<-unix-timestamp timestamp (tz-offset 0))
  (time-utc->date (srfi-19-time<-unix-timestamp timestamp time-utc) tz-offset))

;; Convert a SRFI 19 date object to a timestamp
(def (tai-timestamp<-date date)
  (tai-timestamp<-srfi-19-time (date->time-tai date)))

(def (unix-timestamp<-date date)
  (unix-timestamp<-srfi-19-time (date->time-utc date)))

;; A string for the date represented by this timestamp.
;; The format defaults to ISO 8601 format with nanoseconds.
(def (string<-tai-timestamp tai-timestamp (format-string "~Y-~m-~dT~k:~M:~S.~N~z"))
  (date->string (date<-tai-timestamp tai-timestamp) format-string))

(def (string<-unix-timestamp unix-timestamp (format-string "~Y-~m-~dT~k:~M:~S.~N~z"))
  (date->string (date<-unix-timestamp unix-timestamp) format-string))

;; Parse a timestamp as per SRFI 19.
;; The format defaults to ISO 8601 format with nanoseconds.
(def (tai-timestamp<-string string (format-string "~Y-~m-~dT~k:~M:~S.~N~z"))
  (tai-timestamp<-date (string->date string format-string)))

(def (unix-timestamp<-string string (format-string "~Y-~m-~dT~k:~M:~S.~N~z"))
  (unix-timestamp<-date (string->date string format-string)))

;; Get the timestamp from a YYYYMMDD string
(def (tai-timestamp<-yyyymmdd yyyymmdd)
  (tai-timestamp<-string yyyymmdd "~Y~m~d"))

(def (unix-timestamp<-yyyymmdd yyyymmdd)
  (unix-timestamp<-string yyyymmdd "~Y~m~d"))

;; Display and parse a timestamp
(def display-timestamp display)
(def expect-timestamp expect-natural)


;; Partial support for tai, which is a TAI timestamp at second resolution only
(def (current-tai-time) (time-second (current-time time-tai)))
(def (tai-time<-tai-timestamp timestamp) (floor-quotient timestamp one-second))
(def (tai-timestamp<-tai-time tai) (* tai one-second))
(def (time-tai<-tai-time tai) (make-time time-tai 0 tai))
(def (srfi-19-time<-tai-time tai type) (ensure-srfi-19-time-type (time-tai<-tai-time tai) type))
(def (tai-time<-time-tai srfi-19-time) (assert! (equal? (time-type srfi-19-time) time-tai)) (time-second srfi-19-time))
(def (tai-time<-srfi-19-time srfi-19-time) (time-second (ensure-srfi-19-time-tai srfi-19-time)))
(def (unix-time<-tai-time tai) (unix-time<-srfi-19-time (time-tai<-tai-time tai)))
(def (tai-time<-unix-time unix-time) (tai-time<-srfi-19-time (time-utc<-unix-time unix-time)))
(def (tai-time<-date date) (tai-time<-time-tai (date->time-tai date)))
(def (date<-tai-time tai (tz-offset 0)) (time-tai->date (time-tai<-tai-time tai) tz-offset))
(def (tai-time<-string string (format-string "~Y-~m-~dT~k:~M:~S~z"))
  (tai-time<-date (string->date string format-string)))
(def (string<-tai-time tai (format-string "~Y-~m-~dT~k:~M:~S~z"))
  (date->string (date<-tai-time tai) format-string))


;; What the current system believes was the TAI-UTC adjustment at given timestamp
;; If the system is out-of-date, that might be less than the real one by OFFSET,
;; but the system is presumably well-synchronized in UTC time via NTP, which means
;; all its timestamps are OFFSET*ONE_SECOND too low.
;; TODO: access the innards of SRFI-19 directly for its leap second table.
(def (adjustment<-tai-time tai) (- tai (unix-time<-tai-time tai)))
(def (adjustment<-tai-timestamp timestamp) (adjustment<-tai-time (tai-time<-tai-timestamp timestamp)))


;;; Additional support for date
(def (string<-date date (format-string "~4"))
  (date->string date format-string))

(def (date<-string string (format-string "~Y-~m-~dT~k:~M:~S.~N~z"))
  (string->date string format-string))

;;; Support for UNIX time.
(def (current-unix-time)
  (time-second (current-time time-utc)))

;; Get a SRFI 19 date object from a string in the YYYYMMDD format (Yay Y10K problem!)
;; NB: We always use TAI time in data structures.
;; Also all our servers use UTC as their default system "timezone".
(def (date<-yyyymmdd yyyymmdd)
  (string->date yyyymmdd "~Y~m~d"))

;; Date object for Unix Epoch, for unix-time
(def unix-epoch-date (date<-yyyymmdd "19700101"))

;; Offset in actual seconds for Unix Epoch (should be 0)
(def unix-epoch-offset (time-second (date->time-utc unix-epoch-date)))
(assert! (= unix-epoch-offset 0))

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

(def (srfi-19-time<-unix-time unix-time (type time-utc))
  (ensure-srfi-19-time-type (time-utc<-unix-time unix-time) type))

;; Get Unix time from a SRFI 19 time-utc structure
(def (unix-time<-time-utc utc)
  (assert! (time-type utc) time-utc)
  (let* ((sec (- (time-second utc) unix-epoch-offset))
         (nanosec (time-nanosecond utc)))
    (if (zero? nanosec)
      sec
      (+ sec (* nanosec 1e-9)))))

(def (unix-time<-srfi-19-time srfi-19-time)
  (unix-time<-time-utc (ensure-srfi-19-time-type srfi-19-time time-utc)))

;; TODO: optimize by directly calling leap second deltas from underlying SRFI 19 implementation
(def (tai-timestamp<-unix-time unix-time)
  (tai-timestamp<-srfi-19-time (srfi-19-time<-unix-time unix-time time-tai)))

;; TODO: optimize by directly calling leap second deltas from underlying SRFI 19 implementation
;; and/or pass an explicit adjustment.
(def (unix-time<-tai-timestamp timestamp)
  (unix-time<-srfi-19-time (srfi-19-time<-tai-timestamp timestamp)))

;; Get Unix time from a SRFI 19 date structure
(def (unix-time<-date date)
  (unix-time<-time-utc (date->time-utc date)))

;; Get a SRFI 19 date structure from a Unix time
(def (date<-unix-time u (tz-offset 0))
  (time-utc->date (time-utc<-unix-time u) tz-offset))

;; Get a string from a Unix time. The format defaults to ISO 8601.
(def (string<-unix-time u (format-string "~4") (tz-offset 0))
  (date->string (date<-unix-time u tz-offset) format-string))

;; Get a Unix time from string. The format defaults to ISO 8601.
(def (unix-time<-string i (format-string "~Y-~m-~dT~H:~M:~SZ"))
  (unix-time<-date (string->date i format-string)))

;; Get Unix time from a YYYYMMDD string
(def (unix-time<-yyyymmdd yyyymmdd)
  (unix-time<-date (date<-yyyymmdd yyyymmdd)))


;;; Sleeping
(def (sleep nanoseconds)
  (thread-sleep! (* 1e-9 nanoseconds)))

;; Minimum meaningful sleep quantum, in seconds. Matches the precision of our clock.
(def +sleep-quantum-in-nanoseconds+ 1000)


;;; Miscellaneous
(def min-timestamp 0) ;; should it be 63072010 for 1972-01-01T00:00:10Z, the start of modern TAI?
(def max-timestamp (- (expt 2 63) 1)) ;; NB: Bug in year 2262, when storing in signed int64.
(def time-quantum (make-time time-duration 0 1)) ;; a SRFI-19 object, for 1s



;;; Periodicity

;; Given a day, return the start of the day
(def (date-start-of-day date)
  (make-date 0 0 0 0 (date-day date) (date-month date) (date-year date) (date-zone-offset date)))

(def (time-utc-start-of-day time-utc)
  (date->time-utc (date-start-of-day (time-utc->date time-utc))))

(def (unix-time-start-of-day unix-time)
  (unix-time<-time-utc (time-utc-start-of-day (time-utc<-unix-time unix-time))))

(def (unix-timestamp-start-of-day unix-time)
  (unix-timestamp<-srfi-19-time (time-utc-start-of-day (time-utc<-unix-time unix-time))))

;;; Get start of day for a given timestamp
(def (tai-timestamp-start-of-day timestamp)
  (tai-timestamp<-srfi-19-time (time-utc-start-of-day (srfi-19-time<-tai-timestamp timestamp time-utc))))

(def (date-string<-unix-time unix-time)
  (string<-unix-time unix-time "~Y-~m-~d"))

;; Like date-string<-timestamp but caching the previous answer.
;; NB: closures created by this function are not thread-safe.
(def (caching-date-string<-unix-time)
  (let ((previous-date-start 0)
        (previous-date-end 86399) ;; we use <= and assume integers.
        (date-string "1970-01-01"))
    (λ (unix-time)
      (unless (<= previous-date-start unix-time previous-date-end)
        (set! previous-date-start (unix-time-start-of-day unix-time))
        (set! previous-date-end (+ previous-date-start 86399))
        (set! date-string (date-string<-unix-time previous-date-start)))
      date-string)))

;;; Semester
;; Given a day, return the start of the n-month period, where n must divide 12 (1 2 3 4 6)
(def allowed-n-month-periods '(1 2 3 4 6))
(def (date-start-of-n-month-period date n)
  (assert! (member n allowed-n-month-periods))
  (def month (date-month date))
  (def start-month (- month (modulo (- month 1) n)))
  (make-date 0 0 0 0 1 start-month (date-year date) 0))

;; NB: This assumes the day-of-month is valid after adjustment,
;; but also hours (hello, timezones, daylights-saving time, and
;; date-and-location-dependent political adjustments to them),
;; and seconds (hello, leap-seconds and changing tables thereof).
;; There be dragons outside these operating parameters:
;; We copy preserve the nanosecond, second, minute, hour, day and zone-offset of the date,
;; but really they should be 0 0 0 0 1 0.
;; But it should work for what we care about: days that start of semesters, at midnight.
;; TODO: identify "the right thing" for seconds, minutes, hours, days, and do it.
;; Add what you can, then clip to allowed values? What when negative offsets?
;; What when mix of positive and negative offsets?
(def (roll-date date months: (months 0) years: (years 0))
  (def month (+ (date-month date) months))
  (def new-month (+ 1 (modulo (- month 1) 12)))
  (def new-year (+ (date-year date) (floor-quotient (- month 1) 12) years))
  (make-date (date-nanosecond date) (date-second date) (date-minute date) (date-hour date)
             (date-day date) new-month new-year (date-zone-offset date)))

(def (time-utc-start-of-n-month-period srfi-19-time n (tz-offset 0))
  (date->time-utc (date-start-of-n-month-period (time-utc->date srfi-19-time tz-offset) n)))

(def (unix-time-start-of-n-month-period unix-time n (tz-offset 0))
  (unix-time<-time-utc (time-utc-start-of-n-month-period (time-utc<-unix-time unix-time) n tz-offset)))

(def (unix-timestamp-start-of-n-month-period timestamp n (tz-offset 0))
  (unix-timestamp<-srfi-19-time (time-utc-start-of-n-month-period (srfi-19-time<-unix-timestamp timestamp time-utc) n tz-offset)))

(def (tai-timestamp-start-of-n-month-period timestamp n (tz-offset 0))
  (tai-timestamp<-srfi-19-time (time-utc-start-of-n-month-period (srfi-19-time<-tai-timestamp timestamp time-utc) n tz-offset)))


;; Like adjustment<-tai-time but caching the previous answer.
;; This assumes TAI-UTC only changes just the second before January 1st UTC or July 1st UTC.
;; NB: closures created by this function are not thread-safe.
(def (caching-adjustment<-tai-time)
  (let ((period-start 0)
        (period-end -1) ;; we use <= and assume integers.
        (adjustment 0)
        (last-adjustment 0))
    (λ (tai)
      (unless (<= period-start tai period-end)
        (let* ((date (date<-tai-time tai))
               (semester-start (date-start-of-n-month-period date 6))
               (next-semester-start (roll-date semester-start months: 6)))
          (set! period-start (tai-time<-date semester-start))
          (set! period-end (- (tai-time<-date next-semester-start) 1))
          (set! adjustment (adjustment<-tai-time period-start))
          (when (= tai period-end)
            (let ((next-adjustment (adjustment<-tai-time (+ 1 period-end))))
              (when (> next-adjustment adjustment) ;; it will be n+1
                (set! period-start period-end)
                (set! period-end (- (tai-time<-date (roll-date semester-start years: 1))
                                    2)) ; 2 is conservative, so we don't check a third time
                (set! adjustment next-adjustment))))))
      adjustment)))

;;; Periods.
;;
;; Return the start of a period specified by periodicity, random timestamp in the period, and
;; number of additional periods to add (or remove if negative) to the otherwise specified period.
;; Periodicity can be a number (in nanoseconds), or a symbol: week, month, year
;; Periodicity := (Or Nat 'month 'year)
;; Periods assume "official seconds" that skip leap second.
;; Each period is an interval that start at the period-start (included) and
;; ends at the period-start of the next interval (with additional-periods +1).
;; NB: our time intervals include the start exclude the end.
;;
;; TODO: adjust between TAI and whatever timezone we care about?
;;
;; UnixTimestamp <- Periodicity UnixTimestamp Integer
(def (period-start periodicity unix-timestamp (additional-periods 0))
  (match periodicity
    ((? exact-integer?) (* (+ (floor-quotient unix-timestamp periodicity) additional-periods) periodicity))
    ;; Our weeks start on Monday. The Epoch was a Thursday.
    ('week (- (* (+ (floor-quotient (+ unix-timestamp three-days) one-week) additional-periods) one-week) three-days))
    ('month (let* ((date (date<-unix-timestamp unix-timestamp))
                   (months (+ (date-month date) (* 12 (date-year date)) additional-periods -1))
                   (month (+ (modulo months 12) 1))
                   (year (floor-quotient months 12)))
              (unix-timestamp<-date (make-date 0 0 0 0 1 month year 0))))
    ('year (let ((date (date<-unix-timestamp unix-timestamp)))
             (unix-timestamp<-date (make-date 0 0 0 0 1 1 (+ (date-year date) additional-periods) 0))))
    ((? object?) (period-start {periodicity periodicity} unix-timestamp additional-periods))))

;; The next period, which is also the end of the current period.
(def (period-next periodicity unix-timestamp)
  (period-start periodicity unix-timestamp +1))

;; Count the number of periods of given periodicity from one period to the other,
;; each designated by the unix-timestamp at the start of the period (NB: may be off if not the case)
;; i.e. for integer periodicity, (unix-timestamp2 - unix-timestamp1)/periodicity
(def (period-difference periodicity unix-timestamp1 unix-timestamp2)
  (match periodicity
    ((? exact-integer?) (floor-quotient (- unix-timestamp2 unix-timestamp1) periodicity))
    ('week (floor-quotient (- unix-timestamp2 unix-timestamp1) one-week))
    ('month (let* ((date1 (date<-unix-timestamp unix-timestamp1))
                   (months1 (+ (date-month date1) (* 12 (date-year date1))))
                   (date2 (date<-unix-timestamp unix-timestamp2))
                   (months2 (+ (date-month date2) (* 12 (date-year date2)))))
              (- months2 months1)))
    ('year (- (date-year (date<-unix-timestamp unix-timestamp2)) (date-year (date<-unix-timestamp unix-timestamp1))))
    ((? object?) (period-difference {periodicity periodicity} unix-timestamp1 unix-timestamp2))))

;; Similar to period-start, but with ceiling instead of floor
(def (period-after periodicity unix-timestamp)
  (let ((start (period-start periodicity unix-timestamp)))
    (if (= start unix-timestamp) start (period-next periodicity start))))

(def (periodicity? x)
  (or (and (exact-integer? x) (< 0 x))
      (member x '(week month year))))

(def (sleep-until target-timestamp
                  sleep: (sleep_ sleep)
                  current-timestamp: (current-timestamp current-tai-timestamp))
  (let* ((now (current-timestamp))
         (duration (- target-timestamp now)))
    (when (positive? duration)
      (sleep_ duration))))

;; This function keeps indefinitely calling a thunk on a periodic basis.
;; The function will be called at every "beat" defined by the timestamp being divisible by the period.
;; If running the thunk takes more time than the period, then on-beat-skip is called
;; and the next call is scheduled for the next beat after on-beat-skip returns.
(def (periodically period-in-nanoseconds thunk
                   sleep: (sleep_ sleep)
                   current-timestamp: (current-timestamp current-tai-timestamp)
                   on-beat-skip: (on-beat-skip void))
  (let ((target-timestamp (ceiling-align (current-timestamp) period-in-nanoseconds)))
    (while #t
      (thunk)
      (increment! target-timestamp period-in-nanoseconds)
      (when (> (current-timestamp) target-timestamp)
        (on-beat-skip)
        (set! target-timestamp (ceiling-align (current-timestamp) period-in-nanoseconds)))
      (sleep-until target-timestamp sleep: sleep_ current-timestamp: current-timestamp))))
