;;
;;

(export html:parse
        html:title
        html:urls
        html:text)

(import :std/format
        :std/sugar
        (only-in :std/srfi/1 member)
        (only-in :std/srfi/13 string-trim-both)
        :std/srfi/113
        :std/misc/list
        :std/misc/string
        :std/pregexp
        :std/xml)

(def (html:parse text)
  (parse-html text))

(def (html:title dom)
  (car (find-title dom)))

(def (html:urls dom)
  (unique (find-hrefs dom)))

(def (html:text dom)
  (let* ((ignores '(style script iframe *COMMENT* svg))
         (spacers '(a span i b))

         (on-start (lambda (name attrs seed)
                     (cons (if (member name ignores) #f (car seed))
                           (cdr seed))))

         (on-end (lambda (name seed)
                   (cons (if (member name ignores) #t (car seed))
                         (if (not (member name spacers))
                           (str (cdr seed) " ")
                           (cdr seed)))))

         (on-text (lambda (text seed)
                    (if (car seed)
                      (cons #t (str (cdr seed) text))
                      seed)))

         (parser (make-html-parser start-fn: on-start
                                   end-fn: on-end
                                   text-fn: on-text)))

    (chain (parser dom '(#t . ""))
      (cdr <>)
      (string-normalize <>))))

;;-----------------------------------------------------------------------------
;; Implementation details
;;-----------------------------------------------------------------------------

(def find-title (sxpath '(// title *text*)))
(def find-hrefs (sxpath '(// a @ href *text*)))

(def (string-normalize s)
  (chain s
    (string-trim-both <>)
    (pregexp-replace* "\\s+" <> " ")))

(def html-entities
  ;; TODO: Add the other 10,000 later.
  (hash (raquo  #\u00BB)
        (laquo  #\u00AB)))

(def (make-html-parser start-fn: (start-fn (lambda (name attrs seed) seed))
                       end-fn:   (end-fn   (lambda (name seed) seed))
                       text-fn:  (text-fn  (lambda (text seed) seed)))

  (def (make-pairs assoc-list)
    (map (lambda (x) (cons (car x) (if (equal? '() (cdr x)) "" (cadr x)))) assoc-list))

  (def (parse-attributes node)
    (if (and (pair? (cdr node))
             (list? (car (cdr node)))
             (equal? '@ (car (car (cdr node)))))
      (hash ,@(make-pairs (cdr (car (cdr node)))))
      (make-hash-table)))

  (def (lookup-ce node)
    (string (hash-ref html-entities (cadr node) "?")))

  (lambda (html-dom seed)
    (let eval ((node html-dom)
               (seed seed))
      (cond
       ((equal? node '()) seed)
       ((string? node) (text-fn node seed))
       ((list? node) (cond

                      ((equal? (car node) '&)
                       (text-fn (lookup-ce node) seed))

                      ((equal? (car node) '@)
                       seed)

                      ((symbol? (car node))
                       (let* ((attrs (parse-attributes node))
                              (seed1 (start-fn (car node) attrs seed))
                              (seed2 (eval (cdr node) seed1)))
                         (end-fn (car node) seed2)))

                      (else (let ((seed1 (eval (car node) seed)))
                              (eval (cdr node) seed1)))))
       (else (error (format "Don't know how to parse: ~a" node)))))))
