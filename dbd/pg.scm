;;; dbd.pg - PostgreSQL driver
;;;
;;;  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
;;;  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
;;;  See COPYING for terms and conditions of using this software
;;;
;;; $Id: pg.scm,v 1.2 2005/07/21 08:19:06 nel Exp $

(define-module dbd.pg
  (use gauche.collection)
  (use gauche.sequence)
  (use dbi)
  (use srfi-1)
  (export <pg-driver>
	  <pg-connection>
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
          pq-fname      ; pq.so
	  pq-finish	; pq.so
          dbd-make-connection
          dbd-execute
	  dbi-get-value
	  call-with-iterator
	  dbi-close
          ;; compatibility
	  <pg-query>
  	  dbi-make-connection
	  dbi-make-query
	  dbi-execute-query
          ))
(select-module dbd.pg)

;; Loads extension
(dynamic-load "gauche_dbd_pg")

(define-class <pg-driver> (<dbi-driver>) ())

(define-class <pg-connection> (<dbi-connection>)
  ((%connection :init-keyword :connection :init-value #f)))

(define-class <pg-result-set> (<dbi-result-set>)
  ((%result-set :init-keyword :result-set)
   (%status :init-keyword :status)
   (%error :init-keyword :error)
   (%num-rows :init-keyword :num-rows)
   (%columns  :init-value #f)
   (num-cols :getter dbi-column-count :init-keyword :num-cols)
   ))

(define-method dbd-make-connection ((d <pg-driver>) options option-alist . args)
  (define (keywords->option-string)
    (apply
     string-append
     (let loop ((rest args))
       (cond ((null? rest) '())
             ((null? (cdr rest))
              (error "illigal keyword value pair"))
             (else
              (let ((k (car rest))
                    (v (cadr rest)))
                (cons #`",|k|=,|v| " (loop (cddr rest)))))))))
    
  (let* ((conn (make <pg-connection>
                 :driver-name d
                 :open        #t
                 :connection  (pq-connectdb
                               (keywords->option-string)
                               (make <pq-handle>))))
         (status (pq-status (slot-ref conn '%connection))))
    (if (eq? status CONNECTION_BAD)
        (raise (make <dbi-exception>
                 :error-code status
                 :message "Connect Error: Bad Connection"))
        conn)))

(define-method dbd-execute ((c <pg-connection>) (q <dbi-query>) . params)
  (unless (ref c 'open)
    (raise
     (make <dbi-exception> :error-code -2
           :message "connection is closed.")))
  
  (and-let* ((prepared (ref q '%prepared))
             (sql      (apply prepared params))
             (result   (pq-exec sql (ref c '%connection) (make <pq-res>)))
             (status   (pq-result-status result))
             (error    (pq-result-error-message result)))
    
    (case status
      ((PG_NONFATAL_ERROR)
       (raise
        (make <dbi-exception> :error-code status :error-message error)))
      ((PG_FATAL_ERROR)
       (raise
        (make <dbi-exception> :error-code status :error-message error))))
    
    (make <pg-result-set>
      :open #t
      :result-set result
      :status status
      :error error
      :num-rows (pq-ntuples result)
      :num-cols (pq-nfields result))))

(define-method relation-column-names ((r <pg-result-set>))
  (or (ref r '%columns)
      (let* ((result-set (ref r '%result-set))
             (columns    (map (cut pq-fname result-set <>)
                              (iota (ref r 'num-cols)))))
        (set! (ref r '%columns) columns)
        columns)))

(define-method relation-column-getter ((r <pg-result-set>) column . default)
  (let ((i (find-index (cut equal? column <>) (relation-column-names r))))
    (lambda (row)
      (and i (row i)))))

;; (define-method relation-column-setter ((r <pg-result-set>) column)
;;   (let ((i (find-index (cut equal? column <>) (relation-column-names r))))
;;     (lambda (row val)
;;       (and i (set! (ref row i) val)))))

(define-method call-with-iterator ((r <pg-result-set>) proc . option)
   (if (not (slot-ref r 'open))
       (raise (make <dbi-exception> :error-code -4 :message "<pg-result> already closed.")))
   (let ((row-id -1))
    (define (end?)
      (>= (+ row-id 1) (slot-ref r '%num-rows)))
    (define (next)
      (inc! row-id)
      (lambda (n) (pq-get-value row-id n (slot-ref r '%result-set))))
    (proc end? next)))

(define-method dbi-close ((result-set <pg-result-set>))
  (if (not (slot-ref result-set 'open))
      (raise
       (make <dbi-exception> :error-code -5 :message "already closed.")))
  (slot-set! result-set 'open #f))

(define-method dbi-close ((connection <pg-connection>))
  (if (not (slot-ref connection 'open))
      (raise
       (make <dbi-exception> :error-code -7 :message "already closed.")))
  (slot-set! connection 'open #f)
  (pq-finish (slot-ref connection '%connection)))

(define-method dbi-get-value ((proc <procedure>) (n <integer>)) (proc n))

;;;------------------------------------------------------------
;;; Backward compatibility stuff
;;;

(define-class <pg-query> (<dbi-query>)
  ((%connection   :init-keyword :connection)
   (%query-string :init-keyword :query-string)))

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
  (make <pg-query>
    :open #t
    :connection c))

(define-method dbi-execute-query ((q <pg-query>) (query-string <string>))
  (if (not (slot-ref q 'open))
      (raise
       (make <dbi-exception> :error-code -2
	     :message "query has already closed.")))

  (dbd-execute (ref q '%connection)
               (dbi-prepare (ref q '%connection) query-string)))

(define-method dbi-close ((query <pg-query>))
  (if (not (slot-ref query 'open))
      (raise
       (make <dbi-exception> :error-code -6 :message "already closed.")))
  (slot-set! query 'open #f))

;; Epilogue
(provide "dbd/pg")


