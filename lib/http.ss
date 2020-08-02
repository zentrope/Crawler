;;; -*- Gerbil -*-
;;;
;;; Copyright (c) 2020-present Keith Irwin
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.
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
  ;; Return true if the given URL resolves to an HTML page.

  (truthy (string-contains (mime-type url) "text/html")))


(def (http:resolve url)
  ;; Given a URL, return the URL that results after all redirects have
  ;; been followed.

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
  (let ((status (request-status request)))
    (<= 301 status 308)))

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
;; (http:html? "https://amazon.com")
