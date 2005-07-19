;;; dbd.pg - PostgreSQL driver
;;;
;;;  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
;;;  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
;;;  See COPYING for terms and conditions of using this software
;;;
;;; $Id: pg.scm,v 1.1 2005/07/19 00:45:43 shiro Exp $

(define-module dbd.pg
  (use gauche.collection)
  (use dbi)
  (export <pg-driver>
	  <pg-connection>
	  <pg-query>
	  <pg-result-set>
	  <pq-handle>	; pq.so
	  <pq-res>	; pq.so
	  pq-connectdb	; pq.so
	  pq-exec	; pq.so
	  pq-result-status ; pq.so
	  pq-result-error-message ; pq.so
	  pq-ntuples	; pq.so
	  pq-nfields	; pq.so
	  pq-get-value	; pq.so
	  pq-finish	; pq.so
  	  dbi-make-connection
	  dbi-make-query
	  dbi-execute-query
	  call-with-iterator
	  dbi-get-value
	  dbi-close))
(select-module dbd.pg)

;; Loads extension
(dynamic-load "gauche_dbd_pg")

(define-class <pg-driver> (<dbi-driver>) ())

(define-class <pg-connection> (<dbi-connection>)
  ((%connection :init-keyword :connection :init-value #f)))

(define-class <pg-query> (<dbi-query>)
  ((%connection :init-keyword :connection)
   (%query-string :init-keyword :query-string)))

(define-class <pg-result-set> (<dbi-result-set> <collection>)
  ((%result-set :init-keyword :result-set)
   (%status :init-keyword :status)
   (%error :init-keyword :error)
;;   (%row-id :init-keyword :row-id)
   (%num-rows :init-keyword :num-rows)
   (num-cols :getter dbi-column-count :init-keyword :num-cols)))

(define-method dbi-make-connection ((d <pg-driver>) (user <string>)
				    (password <string>) (option <string>))
  (let ((conn
	(make <pg-connection> :driver-name d :open #t
		   :connection (pq-connectdb
		     (string-append
		      (if (> (string-length user) 0) "user="  "")
		      user
		      (if (> (string-length password) 0) " password=" "")
		      password " " option)
		     (make <pq-handle>)))))
    (let ((status (pq-status (slot-ref conn '%connection))))
      (if (eq? status CONNECTION_BAD)
	  (raise (make <dbi-exception>
		   :error-code status
		   :message "Connect Error: Bad Connection"))
	  conn))))

(define-method dbi-make-query ((c <pg-connection>))
  (if (not (slot-ref c 'open))
      (raise
       (make <dbi-exception> :error-code -1
	     :message "connection has already closed.")))
  (make <pg-query> :open #t
	:connection (slot-ref c '%connection)))

(define-method dbi-execute-query ((q <pg-query>) (query-string <string>))
  (if (not (slot-ref q 'open))
      (raise
       (make <dbi-exception> :error-code -2
	     :message "query has already closed.")))
  (let ((result (pq-exec query-string (slot-ref q '%connection) (make <pq-res>))))
    (let ((status (pq-result-status result))
	  (error (pq-result-error-message result)))
      (case status
	((PG_NONFATAL_ERROR)
	 (raise
	  (make <dbi-exception> :error-code status :error-message error)))
	((PG_FATAL_ERROR)
	 (raise
	  (make <dbi-exception> :error-code status :error-message error))))
      (make <pg-result-set> :open #t :result-set result
	    :status status
	    :error error
	    :num-rows (pq-ntuples result)
	    :num-cols (pq-nfields result)))))

(define-method call-with-iterator ((r <pg-result-set>) proc . option)
  (if (not (slot-ref r 'open))
      (raise (make <dbi-exception> :error-code -4 :message "<pg-result> already closed.")))
  (let ((row-id -1))
    (define (end?)  (>= (+ row-id 1) (slot-ref r '%num-rows)))
    (define (next)
      (inc! row-id)
      (let ((proc 
	     (lambda (n)
	       (let ((value (pq-get-value row-id n (slot-ref r '%result-set)))) value)))) proc))
    (proc end? next)))

(define-method dbi-get-value ((proc <procedure>) (n <integer>)) (proc n))

(define-method dbi-close ((result-set <pg-result-set>))
  (if (not (slot-ref result-set 'open))
      (raise
       (make <dbi-exception> :error-code -5 :message "already closed.")))
  (slot-set! result-set 'open #f))

(define-method dbi-close ((query <pg-query>))
  (if (not (slot-ref query 'open))
      (raise
       (make <dbi-exception :error-code -6 :message "already closed.")))
  (slot-set! query 'open #f))

(define-method dbi-close ((connection <pg-connection>))
  (if (not (slot-ref connection 'open))
      (raise
       (make <dbi-exception> :error-code -7 :message "already closed.")))
  (slot-set! connection 'open #f)
  (pq-finish (slot-ref connection '%connection)))

;; Epilogue
(provide "dbd/pg")


