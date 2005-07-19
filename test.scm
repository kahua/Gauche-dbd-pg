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

;; dbi-make-driver のテスト:
;; "pg" ドライバーをロードして
;; クラス <pg-driver> のインスタンスだったら合格
(define pg-driver (dbi-make-driver "pg"))
(test* "dbi-make-driver pg"
       #t
       (is-a? pg-driver <pg-driver>))

;; dbi-make-connection のテスト:
;; <pg-driver>型のインスタンスを引数にしたとき
;; dbi-make-connection の戻り値が 
;; <pg-connection>型のインスタンスだったら合格
;; 注: (sys-getenv "USER")で取得した現在のユーザーがパスワードなしで
;;     PostgreSQLのデフォルトデータベースに接続できる必要がある。
(define current-user (sys-getenv "USER"))
(define pg-connection
  (dbi-make-connection pg-driver current-user "" ""))
(test* "dbi-make-connection <pg-driver>"
       #t
       (is-a? pg-connection <pg-connection>))

;; dbi-make-query のテスト:
;; <pg-connection>型のインスタンスを引数にしたとき
;; dbi-make-queryの戻り値が
;; <pg-query>型のインスタンスだったら合格
(define pg-query (dbi-make-query pg-connection))
(test* "dbi-make-query <pg-connection>"
       #t
       (is-a? pg-query <pg-query>))

;;;; testテーブルをdropしておく
(with-error-handler
    (lambda (e) #t)
  (lambda () (dbi-execute-query pg-query "drop table test")))
;;;; testテーブルを作成しておく
(dbi-execute-query pg-query "create table test (id integer, name varchar)")
;;;; testテーブルにデータをinsertしておく
(dbi-execute-query pg-query
		   "insert into test (id, name) values (10, 'yasuyuki')")
(dbi-execute-query pg-query
		  "insert into test (id, name) values (20, 'nyama')")

;; dbi-execute-query のテスト:
;; <pg-query>型のインスタンスを引数にしたとき
;; dbi-execute-query の戻り値が
;; <pg-result-set>型のインスタンスだったら合格
(define pg-result-set (dbi-execute-query pg-query "select * from test"))
(test* "dbi-execute-query <pg-query>"
       #t
       (is-a? pg-result-set <pg-result-set>))

;; dbi-get-valueのテスト:
;; map の中で pg-get-value を使って <pg-result-set> からすべての行を取得し、
;; あらかじめ insertされた (("10" "yasuyuki") ("20" "nyama")) に等しければ合格
(test* "dbi-get-value with map"
       '(("10" "yasuyuki") ("20" "nyama"))
  (map (lambda (row)
	      (list (dbi-get-value row 0) (dbi-get-value row 1)))
	    pg-result-set))

;; dbi-close <dbi-result-set> のテスト:
;; <pg-result-set>型のインスタンスをcloseして再度アクセスし、
;; <dbi-exception>が発生したら合格
(dbi-close pg-result-set)
(test* "dbi-close <pg-result-set>" *test-error*
       (dbi-close pg-result-set))

;; dbi-close <dbi-query> のテスト:
;; <pg-query>型のインスタンスをcloseして再度アクセスし、
;; <dbi-exception>が発生したら合格
(dbi-close pg-query)
(test* "dbi-close <pg-query>" *test-error*
       (dbi-close pg-query))

;; dbi-close <dbi-connection> のテスト:
;; <pg-connection>型のインスタンスをcloseして再度アクセスし、
;; <dbi-exception>が発生したら合格
(dbi-close pg-connection)
(test* "dbi-close <pg-connection>" *test-error*
       (dbi-close pg-connection))

;; epilogue
(test-end)





