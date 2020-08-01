;;
;;
;;

(export driver:make
        driver:start
        driver:stop
        driver:live?)

(import :std/format
        :std/sugar
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

(def (driver:stop state)
  (log:info "Stopping driver service.")
  (let-hash state
    (close-session .driver-url .session-id))
  state)

;; ----------------------------------------------------------------------------
;; Implementation details
;; ----------------------------------------------------------------------------

(def (close-session driver-url id)
  (let* ((uri (string-append driver-url "/" id))
         (req (http-delete uri)))
    (request-status req)))

(def (get-session-id driver-url)
  (let* ((doc (json-object->string capabilities))
         (heads '(("Content-Type" . "application/json")))
         (req (http-post driver-url headers: heads data: doc))
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