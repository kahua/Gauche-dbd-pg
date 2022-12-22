;;;
;;; Test dbd.pg
;;;

(use gauche.collection)
(use gauche.test)
(use scheme.list)
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
         (dbi-execute query 10 "yasuyuki")
         (dbi-execute query 20 "nyama")
         (dbi-execute query 30 "who's this?")
         #t))

;; query
(test* "dbi-do (select)" #t
       (begin
         (set! *res* (dbi-do *conn* "select * from test"))
         (is-a? *res* <pg-result-set>)))

(test* "dbi-get-value with map"
       '(("10" "yasuyuki") ("20" "nyama") ("30" "who's this?"))
  (map (lambda (row)
         (list (dbi-get-value row 0) (dbi-get-value row 1)))
       *res*))

(test* "relation-column-names"
       '("id" "name")
       (relation-column-names *res*))

(test* "relation-getter with map"
       '(("10" "yasuyuki") ("20" "nyama") ("30" "who's this?"))
       (let ((getter (relation-accessor *res*)))
         (map (lambda (row)
                (list (getter row "id")
                      (getter row "name")))
              *res*)))

(test* "relation-ref with map"
       '(("10" "yasuyuki") ("20" "nyama") ("30" "who's this?"))
       (map (lambda (row)
              (list (relation-ref *res* row "id")
                    (relation-ref *res* row "name")))
            *res*))

;; direct access using pq-send-query and pq-get-result
(let ([raw-conn (pg-connection-handle *conn*)])
  (test* "direct access 1 - pq-send-query"
         1
         (pq-send-query raw-conn
                        "SELECT id FROM test WHERE name = 'yasuyuki'"))

  (test* "direct access 1 - pq-get-result"
         `(,PGRES_TUPLES_OK "10")
         (let* ([r (pq-get-result raw-conn)]
                [s (pq-result-status r)])
           (list s (pq-getvalue r 0 0))))

  (test* "direct access 1 - pq-get-result"
         #f                             ;no more result
         (pq-get-result raw-conn))
  )

;; direct access with pq-set-single-row-mode
(let ([raw-conn (pg-connection-handle *conn*)])
  (pq-send-query raw-conn "SELECT id FROM test ORDER BY id")
  (test* "direct access 2 - pq-set-single-row-mode" 1
         (pq-set-single-row-mode raw-conn))
  (test* "direct access 2 - getting values"
         `(,PGRES_SINGLE_TUPLE "10")
         (let1 r (pq-get-result raw-conn)
           (list (pq-result-status r) (pq-getvalue r 0 0))))
  (test* "direct access 2 - getting values"
         `(,PGRES_SINGLE_TUPLE "20")
         (let1 r (pq-get-result raw-conn)
           (list (pq-result-status r) (pq-getvalue r 0 0))))
  (test* "direct access 2 - getting values"
         `(,PGRES_SINGLE_TUPLE "30")
         (let1 r (pq-get-result raw-conn)
           (list (pq-result-status r) (pq-getvalue r 0 0))))
  (test* "direct access 2 - end of rows"
         `(,PGRES_TUPLES_OK 0)
         (let1 r (pq-get-result raw-conn)
           (list (pq-result-status r) (pq-ntuples r))))
  (test* "direct access 2 - fetch until NULL"
         #f
         (let loop ()
           (and-let1 r (pq-get-result raw-conn)
             (loop))))
  )

;; closing
(test* "dbi-close (result)" #f
       (begin (dbi-close *res*) (dbi-open? *res*)))

(test* "dbi-close (connection)" #f
       (begin (dbi-close *conn*) (dbi-open? *conn*)))

;; epilogue
(test-end :exit-on-failure #t)
