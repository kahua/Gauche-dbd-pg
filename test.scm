;;;
;;; Test dbd.pg
;;;

(use gauche.test)
(use gauche.collection)
(use srfi-1)
(use srfi-13)

(test-start "dbd.pg")
(use dbi)
(use dbd.pg)
(test-module 'dbd.pg)

(test-section "new API")

(define *conn* #f)
(define *res* #f)

(test* "dbi-connect" #t
       (begin
         (set! *conn*
               (dbi-connect "dbi:pg"
                            :username (sys-uid->user-name (sys-getuid))))
         (is-a? *conn* <pg-connection>)))

;; drop test table if there's any.  we can ignore dbi-error in case
;; test table hasn't exist.
(test* "dbi-do (drop table)" #t
       (guard (e
               (<dbi-error> #t))
         (dbi-do *conn* "drop table test")
         #t))

;; create and populate the test table
(test* "dbi-do (create table)" #t
       (begin
         (dbi-do *conn* "create table test (id integer, name varchar)")
         #t))

(test* "dbi-prepare/execute (insert)" #t
       (let1 query
           (dbi-prepare *conn* "insert into test (id, name) values (?, ?)")
         (query 10 "yasuyuki")
         (query 20 "nyama")
         #t))

;; query 
(test* "dbi-do (select)" #t
       (begin
         (set! *res* (dbi-do *conn* "select * from test"))
         (is-a? *res* <pg-result-set>)))

(test* "dbi-get-value with map"
       '(("10" "yasuyuki") ("20" "nyama"))
  (map (lambda (row)
         (list (dbi-get-value row 0) (dbi-get-value row 1)))
       *res*))

(test* "relation-column-names"
       '("id" "name")
       (relation-column-names *res*))

(test* "relation-getter with map"
       '(("10" "yasuyuki") ("20" "nyama"))
       (let ((getter (relation-accessor *res*)))
         (map (lambda (row)
                (list (getter row "id")
                      (getter row "name")))
              *res*)))

(test* "relation-ref with map"
       '(("10" "yasuyuki") ("20" "nyama"))
       (map (lambda (row)
              (list (relation-ref *res* row "id")
                    (relation-ref *res* row "name")))
            *res*))

;; closing
(test* "dbi-close (result)" #f
       (begin (dbi-close *res*) (dbi-open? *res*)))

(test* "dbi-close (connection)" #f
       (begin (dbi-close *conn*) (dbi-open? *conn*)))

;; epilogue
(test-end)





