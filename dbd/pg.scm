;;; dbd.pg - PostgreSQL driver
;;;
;;;  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
;;;  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
;;;  See COPYING for terms and conditions of using this software
;;;

(define-module dbd.pg
  (use gauche.sequence)
  (use dbi)
  (use srfi-1)
  (use util.list)
  (use gauche.mop.singleton)
  (export <pg-driver>
          <pg-connection>
          <pg-result-set>
          <pg-row>

          ;; low-level stuff
          <pg-conn> <pg-result>
          pg-connection-handle
          pq-connectdb pq-finish pq-reset pq-db pq-user pq-pass pq-host
          pq-port pq-tty pq-options pq-status pq-error-message
          pq-backend-pid pq-exec pq-result-status pq-res-status
          pq-result-error-message pq-ntuples pq-nfields pq-fname
          pq-fnumber pq-ftype pq-fsize pq-fmod pq-binary-tuples
          pq-getvalue pq-getisnull pq-cmd-status pq-cmd-tuples pq-oid-status
          pq-clear pq-trace pq-untrace pq-set-notice-processor
          pq-finished? pq-cleared? pq-escape-string

          PGRES_EMPTY_QUERY PGRES_COMMAND_OK PGRES_TUPLES_OK
          PGRES_COPY_OUT PGRES_COPY_IN PGRES_COPY_IN
          PGRES_NONFATAL_ERROR PGRES_FATAL_ERROR
          pq-send-query pq-get-result
          ))
(select-module dbd.pg)

;; Loads extension
(dynamic-load "dbd_pg")

(define-class <pg-driver> (<dbi-driver> <singleton-mixin>) ())

(define-class <pg-connection> (<dbi-connection>)
  ((%handle :init-keyword :handle :init-value #f)))
(define (pg-connection-handle conn)     ;public accessor
  (assume-type conn <pg-connection>)
  (~ conn'%handle))

(define-class <pg-result-set> (<relation> <sequence>)
  ((%pg-result :init-keyword :pg-result)
   (%status :init-keyword :status)
   (%error :init-keyword :error)
   (%num-rows :init-keyword :num-rows)
   (%columns  :init-value #f)
   (num-cols :getter dbi-column-count :init-keyword :num-cols)
   ))

(define-class <pg-row> (<sequence>)
  ((%result-set :init-keyword :result-set)
   (%row-id     :init-keyword :row-id)))

;;
(define-method dbi-make-connection ((d <pg-driver>)
                                    (options <string>)
                                    (option-alist <list>)
                                    . args)
  (define (build-option-string)
    (string-join (map (lambda (p)
                        (format "~a='~a'" (car p)
                                (regexp-replace-all #/'/ (cdr p) "\\'")))
                      (all-options))))

  (define (all-options)
    (append
     (cond-list
      ((get-keyword :username args #f) => (cut cons 'user <>))
      ((get-keyword :password args #f) => (cut cons 'password <>)))
     option-alist))

  (let* ((conn (make <pg-connection>
                 :driver-name d
                 :open        #t
                 :handle  (pq-connectdb (build-option-string))))
         (status (pq-status (ref conn '%handle))))
    (when (eq? status CONNECTION_BAD)
      (error <dbi-error>
             "PostgreSQL connect Error:"
             (pq-error-message (ref conn '%handle))))
    conn))

(define (pg-connection-check c)
  (when (pq-finished? (slot-ref c '%handle))
    (error <dbi-error> "closed connection:" c)))

;; Postgres has prepared statement feature.  Eventually we're going
;; to use it, but for now, we use Gauche's default preparation routine.

(define-method dbi-execute-using-connection ((c <pg-connection>)
                                             (q <dbi-query>) params)
  (pg-connection-check c)
  (let* ((h  (slot-ref c '%handle))
         (prepared (slot-ref q 'prepared))
         (result (pq-exec h (apply prepared params)))
         (status (pq-result-status result)))
    (when (memv status `(,PGRES_NONFATAL_ERROR ,PGRES_FATAL_ERROR))
      (error <dbi-error> (pq-result-error-message result)))
    (make <pg-result-set>
      :pg-result result
      :status status
      :error error
      :num-rows (pq-ntuples result)
      :num-cols (pq-nfields result))))

(define (pg-result-set-check r)
  (when (pq-cleared? (slot-ref r '%pg-result))
    (error <dbi-error> "closed result set:" r)))

(define-method dbi-escape-sql ((c <pg-connection>) str)
  (pq-escape-string str))

;;
;; Relation API
;;
(define-method relation-column-names ((r <pg-result-set>))
  (pg-result-set-check r)
  (or (ref r '%columns)
      (let1 columns
          (map (cut pq-fname (slot-ref r '%pg-result) <>)
               (iota (slot-ref r 'num-cols)))
        (set! (slot-ref r '%columns) columns)
        columns)))

(define-method relation-accessor ((r <pg-result-set>))
  (pg-result-set-check r)
  (let1 column-names (relation-column-names r)
    (lambda (row column . maybe-default)
      (cond
       ((find-index (cut equal? column <>) column-names)
        => (lambda (i)
             (pq-getvalue (slot-ref (slot-ref row '%result-set) '%pg-result)
                          (slot-ref row '%row-id) i)))
       ((pair? maybe-default) (car maybe-default))
       (else (error "pg-result-set: invalid column name:" column))))))

(define-method relation-rows ((r <pg-result-set>))
  (coerce-to <list> r)) ;; use call-with-iterator

(define-method referencer ((r <pg-result-set>))
  (lambda (row ind . fallback)
    (if (or (< ind 0) (<= (slot-ref r '%num-rows) ind))
      (get-optional fallback (error "index out of range:" ind))
      (make <pg-row> :result-set r :row-id ind))))

(define-method call-with-iterator ((r <pg-result-set>) proc . option)
  (pg-result-set-check r)
  (let ((row-id -1))
    (define (end?)
      (>= (+ row-id 1) (slot-ref r '%num-rows)))
    (define (next)
      (inc! row-id)
      (make <pg-row> :result-set r :row-id row-id))
    (proc end? next)))

(define-method call-with-iterator ((row <pg-row>) proc . option)
  (let* ((result   (slot-ref row '%result-set))
         (num-cols (slot-ref result 'num-cols))
         (col-id -1))
    (proc (lambda () (>= (+ col-id 1) num-cols))
          (lambda ()
            (inc! col-id)
            (pq-getvalue (slot-ref result '%pg-result)
                         (slot-ref row '%row-id) col-id)))))

(define-method referencer ((row <pg-row>))
  dbi-get-value)

(define-method dbi-get-value ((row <pg-row>) index)
  (pg-result-set-check (slot-ref row '%result-set))
  (pq-getvalue (slot-ref (slot-ref row '%result-set) '%pg-result)
               (slot-ref row '%row-id) index))

(define-method dbi-open? ((result-set <pg-result-set>))
  (not (pq-cleared? (ref result-set '%pg-result))))

(define-method dbi-close ((result-set <pg-result-set>))
  (unless (pq-cleared? (ref result-set '%pg-result))
    (pq-clear (ref result-set '%pg-result))))

(define-method dbi-open? ((connection <pg-connection>))
  (not (pq-finished? (slot-ref connection '%handle))))

(define-method dbi-close ((connection <pg-connection>))
  (unless (pq-finished? (slot-ref connection '%handle))
    (pq-finish (slot-ref connection '%handle))))

;; Epilogue
(provide "dbd/pg")
