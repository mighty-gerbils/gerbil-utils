;; Querying the WHOIS database
(export #t)

(import
  :std/format :std/misc/list :std/misc/ports :std/misc/process :std/pregexp :std/srfi/1 :std/sugar
  :clan/utils/assert :clan/utils/base :clan/utils/list)

(def (valid-domain? name)
  (and (pregexp-match "^(((?!-))(xn--)?[a-z0-9-_]{0,61}[a-z0-9]{1,1}[.])*(xn--)?([-a-z0-9]{1,61}|[-a-z0-9]{1,30}[.][a-z]{2,})$" name) #t))

(def (whois-domain name)
  (assert! (valid-domain? name))
  (def whois (run-process ["whois" name] coprocess: read-all-as-lines))
  (nest
   (let/cc return)
   (if (pregexp-match "^No match for \".*\"." (first whois)) #f)
   (with-list-builder (c! results))
   (for-each! whois) (λ (line))
   (cond
    ((pregexp-match "^ *([-/ A-Za-z0-9]+):(?: (.*))?$" line)
     => (λ-match ([all key val] (c! (cons key val)))))
    ((pregexp-match ">>> Last update of WHOIS database: (.*) <<<" line)
     => (λ-match ([all val] (c! (cons "Last update of whois database" val)) (return (results)))))
    (else (error "bad whois response" line whois)))))
