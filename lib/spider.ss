;;; -*- mode: gerbil; -*-
;;;


(export spider:make
        spider:crawl)

(import :crawler/lib/driver
        :crawler/lib/parser
        :std/format
        :std/sugar
        :std/srfi/13)

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



;;-----------------------------------------------------------------------------
;; Implementation details
;;-----------------------------------------------------------------------------

(def (normalize-urls base urls)
  (let* ((remove-hashes (lambda (urls)
                          (filter (lambda (c) (not (string-contains c "#"))) urls)))
         (remove-remotes (lambda (urls)
                           (filter (lambda (c) (string-prefix? c base)) urls))))
    ;;
    (chain urls
      (remove-remotes <>)
      (remove-hashes <>))
    ;;
    ;;
    ))


(def (try-stuff text)
  (let* ((dom (html:parse text))
         (title (html:title dom))
         (urls (html:urls dom))
         (text (html:text dom)))
    (printf "title '~a'~%" title)
    (for-each (lambda (u)
                (printf "url: '~a'~%" u)) urls)
    (printf "text: ~a~%" text)))
