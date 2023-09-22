;; -*- Gerbil -*-
;; Variant of std/iter where you can peek at the current value in the iterator.

(export
  keep-raising
  peekable-iterator :peekable-iter
  peekable-iterator-end? peekable-iterator-peek peekable-iterator-next! peekable-iterator-fini!
  end? peek next! fini!)

(import
  :gerbil/gambit
  :std/coroutine :std/generic :std/iter :std/sugar
  ./base)

;; Keep raising
(def (keep-raising e) (λ _ (while #t (raise e))))

;; status can be #f (no current value cached), 'value (value cached), 'exception (exception cached), 'end (reached the end already)
(defstruct peekable-iterator (iterator current status)
  constructor: :init! unchecked: #t final: #t)

(defmethod {:init! peekable-iterator}
  (lambda (self iterator (current #f) (status #f))
    (struct-instance-init! self iterator current status)))

(defmethod (:iter (pi peekable-iterator))
  (case (&peekable-iterator-status pi)
    ((#f) (&peekable-iterator-iterator pi))
    ((end) (:iter '()))
    ((exception) (in-coroutine (keep-raising (&peekable-iterator-current pi))))
    ((value) (in-coroutine (λ () (yield (&peekable-iterator-current pi))
                              (for (val (&peekable-iterator-iterator pi)) (yield val)))))))

(defgeneric :peekable-iter)
(defmethod (:peekable-iter (pi peekable-iterator)) pi)
(defmethod (:peekable-iter (it iterator)) (make-peekable-iterator it))
(defmethod (:peekable-iter (obj <pair>)) (make-peekable-iterator (:iter obj)))
(defmethod (:peekable-iter (obj <null>)) (make-peekable-iterator (:iter obj)))
(defmethod (:peekable-iter (obj <vector>)) (make-peekable-iterator (:iter obj)))
(defmethod (:peekable-iter (obj <string>)) (make-peekable-iterator (:iter obj)))
(defmethod (:peekable-iter (obj <hash-table>)) (make-peekable-iterator (:iter obj)))
(defmethod (:peekable-iter (obj <procedure>)) (make-peekable-iterator (:iter obj)))
(defmethod (:peekable-iter (obj <port>)) (make-peekable-iterator (:iter obj)))
(defmethod (:peekable-iter (obj <object>)) (make-peekable-iterator (:iter obj)))

(def (peekable-iterator-fill-cache pi)
  (let ((it (&peekable-iterator-iterator pi)))
    (try
     (let ((next (iter-next! it)))
       (set! (&peekable-iterator-current pi) next)
       (if (eq? next iter-end)
         (begin
           (set! (&peekable-iterator-status pi) 'end)
           (iter-fini! it))
         (set! (&peekable-iterator-status pi) 'value)))
     (catch (e)
       (set! (&peekable-iterator-current pi) e)
       (set! (&peekable-iterator-status pi) 'exception)
       (iter-fini! it)))))

(def (peekable-iterator-ensure-cache-filled pi)
  (unless (&peekable-iterator-status pi)
    (peekable-iterator-fill-cache pi)))

(def (read-cache status current)
  (case status
    ((end) iter-end)
    ((value) current)
    ((exception) (raise current))))

(def (peekable-iterator-read-cache pi)
  (read-cache (&peekable-iterator-status pi) (&peekable-iterator-current pi)))

(def (peekable-iterator-end? pi)
  (peekable-iterator-ensure-cache-filled pi)
  (eq? (&peekable-iterator-status pi) 'end))

(def (peekable-iterator-peek pi (default iter-end))
  (peekable-iterator-ensure-cache-filled pi)
  (peekable-iterator-read-cache pi))

(def (peekable-iterator-next! pi (default iter-end))
  (peekable-iterator-ensure-cache-filled pi)
  (let ((status (&peekable-iterator-status pi))
        (current (&peekable-iterator-current pi)))
    (when (eq? status 'value) ;; 'end and 'exception stick, 'value doesn't
      (set! (&peekable-iterator-status pi) #f))
    (read-cache status current)))

(def (peekable-iterator-fini! pi)
  (case (&peekable-iterator-status pi)
    ((#f value)
     (set! (&peekable-iterator-status pi) 'end)
     (iter-fini! (&peekable-iterator-iterator pi)))
    ((exception end) (void))))

(defgeneric peek)
(defmethod (peek (pi peekable-iterator)) (peekable-iterator-peek pi))

(defgeneric end?)
(defmethod (end? (it iterator)) (iter-end? it))
(defmethod (end? (pi peekable-iterator)) (peekable-iterator-end? pi))

(defgeneric next!)
(defmethod (next! (it iterator)) (iter-next! it))
(defmethod (next! (pi peekable-iterator)) (peekable-iterator-next! pi))

(defgeneric fini!)
(defmethod (fini! (it iterator)) (iter-fini! it))
(defmethod (fini! (pi peekable-iterator)) (peekable-iterator-fini! pi))
