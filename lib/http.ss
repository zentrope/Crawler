;;
;;
;;

(export http:html?)

(import :std/net/request
        :std/srfi/13
        :std/sugar)

(def (http:html? url)
  (truthy (string-contains (mime-type url) "text/html")))

;; ----------------------------------------------------------------------------
;; Implementation Details
;; ----------------------------------------------------------------------------

(def (truthy value)
  (if (equal? value #f) #f #t))

(def headers
  '(("Accept" . "*/*")))

(def (mime-type url)
  (chain (http-head url headers: headers)
    (request-headers <>)
    (list->hash-table <>)
    (hash-ref <> "Content-Type")))

#| repl
(mime-type "https://zentrope.com")
(http:html? "https://zentrope.com/favicon.ico")
|#