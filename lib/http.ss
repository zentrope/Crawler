;;; -*- mode: gerbil; -*-
;;;

(export http:html?
        http:resolve)

(import (only-in :std/net/request
                 http-post http-get http-head
                 request-headers request-status)
        (only-in :std/srfi/13
                 string-contains)
        (only-in :std/sugar
                 chain))

(def (http:html? url)
  ;;
  ;; Return true if the given URL resolves to an HTML page.
  ;;
  (chain (mime-type url)
    (string-contains <> "text/html")
    (truthy <>)))

(def (http:resolve url)
  ;;
  ;; Given a URL, return the URL that results after all redirects have
  ;; been followed.
  ;;
  (let loop ((url url))
    (let ((result (http-get url headers: headers redirect: #f)))
      (if (not (redirect? result))
        url
        (loop (request-header result "Location"))))))

;; ----------------------------------------------------------------------------
;; Implementation Details
;; ----------------------------------------------------------------------------

(def (request-header request name)
  (assget name (request-headers request)))

(def (redirect? request)
  (<= 301 (request-status request) 308))

(def (truthy value)
  (if (equal? value #f) #f #t))

(def headers
  '(("Accept" . "*/*")))

(def (mime-type url)
  (chain (http-head url headers: headers)
    (request-headers <>)
    (list->hash-table <>)
    (hash-ref <> "Content-Type")))

;; (http:resolve "http://amazon.com")
;; (mime-type "https://amazon.com")
;; (http:html? "https://www.amazon.com")
