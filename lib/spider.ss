;;; -*- mode: gerbil; -*-
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

(export spider:make
        spider:crawl)

(import :crawler/lib/driver
        :crawler/lib/parser
        :std/format
        :std/sugar
        :std/xml)

(def (spider:make driver: driver)
  (let ((state (make-hash-table)))
    (hash-put! state 'driver driver)
    state))

(def (spider:crawl state url: url handler: handler)
  (let-hash state
    (let* ((text (driver:download .driver url)))
      (try-stuff text)
      ;;(handler text)
      ))
  state)

(def (try-stuff text)
  (let* ((dom (html:parse text))
         (title (html:title dom))
         (urls (html:urls dom))
         (text (html:text dom)))
    (printf "title '~a'~%" title)
    (for-each (lambda (u)
                (printf "url: '~a'~%" u)) urls)
    (printf "text: ~a~%" text)))
