(import
  (only-in :std/error deferror-class raise/context exception-context)
  (only-in :std/generic type-of)
  (only-in :std/sugar defrule with-id))

(import :std/misc/ports)

(export PrivateDataError PrivateDataError? defprivate-struct)

(deferror-class PrivateDataError ())

;; TODO: allow debugging inside in some cases?
;;(def debug-private-data (make-parameter #f))

(def (call-with-private-data where x msg thunk)
  (with-catch
   (lambda (e)
     (raise (PrivateDataError
             (as-string "Error processing private data: " msg)
             where: where
             irritants: [x (type-of e)])))
   thunk))

(defrule (new-struct-funs name (slots ...))
  (let ()
    (defstruct name (slots ...))
    (with-id name ((name? #'name "?")
                   (make-name "make-" #'name))
      (def (call-with-name fun x) (with ((name slots ...) x) (fun slots ...)))
      (values make-name call-with-name name?))))

(defrule (defprivate-struct name (slots ...))
  (with-id name ((name? #'name "?")
                 (with-name "with-" #'name))
    (define-values (name call-with-name name?) (new-struct-funs name (slots ...)))
    (def (call-with-private fun x where (msg ""))
      (call-with-private-data where x msg (cut call-with-name fun x)))
    (defrule (with-name ((vars (... ...)) x . msg?) body (... ...))
      (call-with-private
       (lambda (vars (... ...)) body (... ...)) x (exception-context x) . msg?))))
