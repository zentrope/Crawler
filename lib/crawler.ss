;;; -*- mode: gerbil; -*-
;;;

(import :crawler/lib/driver
        :crawler/lib/spider
        :crawler/lib/support
        (only-in :gerbil/gambit display-exception)
        :std/sugar
        :std/format)

(export main)

(def chrome-driver-url
  "http://localhost:9515/session")

(def (make-system)
  (let* ((system (make-hash-table))
         (driver (driver:make url: chrome-driver-url))
         (spider (spider:make driver: driver)))
    (hash-put! system 'driver driver)
    (hash-put! system 'spider spider)
    system))

(def (start system)
  (log:info "Starting system.")
  (let-hash system
    (driver:start .driver)
    (try
     (spider:crawl .spider
                   url: "https://zentrope.com"
                   handler: (lambda (text) (displayln text)))
     (catch (e)
       (display-exception e)
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