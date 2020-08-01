;;
;;
;;

(import :crawler/lib/driver
        :crawler/lib/support
        :std/sugar
        :std/format)

(export main)

(def chrome-driver-url
  "http://localhost:9515/session")

(def driver-service
  (lambda ()
    (driver:make url: chrome-driver-url)))

(def (make-system)
  (let ((system (make-hash-table)))
    (hash-put! system 'driver (driver-service))
    system))

(def (start system)
  (log:info "Starting system.")
  (let-hash system
    (driver:start .driver)
    (try
     (let (text (driver:download .driver "https://zentrope.com"))
       (displayln text))
     (catch (e)
       (log:error "ERROR: ~a" e)))))

(def (stop system)
  (log:info "Stopping system.")
  (let-hash system
    (driver:stop .driver)))

(def (main . args)
  (log:info "Bootstrapping")
  (when (not (driver:live? chrome-driver-url))
    (log:error "The chromedriver '~a' is not running. Start it?" chrome-driver-url)
    (exit 1))
  (let ((lock (lock:make))
        (system (make-system)))
    (sig:add-shutdown-hook 'app (lambda () (lock:release lock)))
    (start system)
    (lock:wait lock)
    (stop system)
    (log:info "Halted.")
    (exit 0)))