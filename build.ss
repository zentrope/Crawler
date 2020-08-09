#!/usr/bin/env gxi

(import :std/make)

(def app-name
  "crawler")

(def libs
  '("lib/support"
    "lib/parser"
    "lib/http"
    "lib/driver"
    "lib/spider"))

(def build-deps
  (append libs '("lib/crawler")))

(def build-exe
  (append libs '((exe: "lib/crawler" ;; "-ld-options" "-lz -lxml2 /usr/local/opt/openssl@1.1/lib/libcrypto.a /usr/local/opt/openssl@1.1/lib/libssl.a"
                       ))))

(def (build-static)
  (append libs `((static-exe: "lib/crawler"
                              "-ld-options"
                              "-lz -lxml2 /usr/local/opt/openssl@1.1/lib/libcrypto.a /usr/local/opt/openssl@1.1/lib/libssl.a"))))


(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (usage)
  (displayln "./build.ss <options>

OPTIONS:
  clean  - remove build detritus
  exe    - build an exe
  static - build a static exe (NOSSL=\"yes\" ./build.ss static)
  deps   - build dependencies
  help   - show this help"))

(def (main . args)
  (match args
    (["clean"]
     (when (file-exists? app-name)
       (delete-file app-name)))

    (["help"]
     (usage))

    (["exe"]
     (make srcdir: srcdir bindir: "." build-exe))

    (["static"]
     (make srcdir: srcdir bindir: "." (build-static)))

    (["deps"]
     (make srcdir: srcdir build-deps))

    ([]  (usage))))
