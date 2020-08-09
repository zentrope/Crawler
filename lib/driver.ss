;;; -*- mode: gerbil; -*-
;;;

(export driver:make
        driver:start
        driver:stop
        driver:download
        driver:live?)

(import :std/format
        :std/sugar
        :std/misc/string
        :std/text/json
        :std/net/request
        :crawler/lib/support)

(def (driver:live? driver-url)
  (try
   (http-get driver-url)
   #t
   (catch (x)
     #f)))

(def (driver:make url: (url "http://localhost:9515/session"))
  (let ((state (make-hash-table)))
    (hash-put! state 'driver-url url)
    state))

(def (driver:start state)
  (log:info "Starting driver service.")
  (let-hash state
    (hash-put! state 'session-id (get-session-id .driver-url)))
  state)

(def (driver:download state url)
  (log:info "Visiting ~a." url)
  (let-hash state
    (visit-url .driver-url .session-id url)
    (download-source .driver-url .session-id)))

(def (driver:stop state)
  (log:info "Stopping driver service.")
  (let-hash state
    (close-session .driver-url .session-id))
  state)

;; ----------------------------------------------------------------------------
;; Implementation details
;; ----------------------------------------------------------------------------

(def json-head
  '(("Content-Type" . "application/json")))

(def (download-source driver-url session-id)
  (let* ((url (str driver-url "/" session-id "/source"))
         (doc (request-json (http-get url))))
    (hash-ref doc 'value)))

(def (visit-url driver-url session-id  visit-url)
  (let* ((url (str driver-url "/" session-id "/url"))
         (doc (json-object->string (hash (url visit-url)))))
    (http-post url headers: json-head data: doc)))

(def (close-session driver-url id)
  (let* ((uri (string-append driver-url "/" id))
         (req (http-delete uri)))
    (request-status req)))

(def (get-session-id driver-url)
  (let* ((doc (json-object->string capabilities))
         (req (http-post driver-url headers: json-head data: doc))
         (results (request-json req)))
    (chain results
      (hash-ref <> 'value)
      (hash-ref <> 'sessionId))))

(def args
  (hash (args '("--headless" "--disable-gpu"))))

(def chrome
  (hash (browserName  "chrome")
        (goog:chromeOptions  args)))

(def capabilities
  (hash (capabilities (hash (firstMatch `(,chrome))))))

;; EOF