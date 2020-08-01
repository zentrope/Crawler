;;
;;
;;

(export lock:lock?
        lock:make
        lock:release
        lock:wait
        log:error
        log:info
        sig:add-shutdown-hook
        sig:rem-shutdown-hook)

(import :gerbil/gambit/ports
        :gerbil/gambit/threads
        :std/format
        :std/misc/channel
        :std/misc/string
        :std/os/signal
        :std/os/signal-handler
        :std/srfi/13
        :std/srfi/19
        :std/sugar)

;; ----------------------------------------------------------------------------
;; Lock wait/release
;; ----------------------------------------------------------------------------

;; TODO: should use :std/misc/completion ?

(def (lock:make)
  (let ((state (make-hash-table)))
    (hash-put! state 'type 'lock)
    (hash-put! state 'channel (make-channel 1))
    state))

(def (lock:lock? state)
  (let-hash state
    (equal? 'lock .type)))

(def (lock:wait state)
  (let-hash state
    (channel-get .channel)))

(def (lock:release state)
  (let-hash state
    (channel-put .channel 'done)
    (channel-close .channel)
    #f))

;; ----------------------------------------------------------------------------
;; Logging
;; ----------------------------------------------------------------------------

(def log-date
  "~Y-~m-~d ~H:~M:~S")

(def tz-offset
  (* -1 7 60 60))

(def (now-string)
  (date->string (current-date tz-offset) "~Y-~m-~d ~H:~M:~S"))

(def (log:msg level fmt . args)
  (let* ((stamp (now-string))
         (thread (string-pad-right (format "~a" (thread-name (current-thread))) 12))
         (lvl (string-pad-right level 5))
         (msg (str stamp " | " lvl " | " thread " | " fmt)))
    (displayln (apply format msg args))
    (force-output)))

(def (log:info fmt . args)
  (apply log:msg "INFO" fmt args))

(def (log:error fmt . args)
  (apply log:msg "ERROR" fmt args))

;; ----------------------------------------------------------------------------
;; Signal handling
;; ----------------------------------------------------------------------------

(def (sig:add-shutdown-hook id thunk)
  (hash-put! handlers id thunk))

(def (sig:rem-shutdown-hook id)
  (hash-remove! handlers id))

(def handlers
  (make-hash-table))

(def (invoke-handlers)
  (let loop ((hs (hash-values handlers)))
    (when (pair? hs)
      ((car hs))
      (loop (cdr hs)))))

(add-signal-handler! SIGINT invoke-handlers)
(add-signal-handler! SIGHUP invoke-handlers)
(add-signal-handler! SIGTERM invoke-handlers)
