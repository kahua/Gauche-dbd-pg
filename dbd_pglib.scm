;;-*-Scheme-*-
;; dbd_pglib.stub - PostgreSQL API
;;
;;  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

(select-module dbd.pg)

(inline-stub
 (.include "dbd_pg.h")

 (declare-stub-type <pg-conn> "PGconn*" "PostgreSQL Connection"
                    "PG_CONN_P" "PG_CONN_UNBOX" "PG_CONN_BOX")
 (declare-stub-type <pg-result> "PGresult*" "PostgreSQL Result"
                    "PG_RESULT_P" "PG_RESULT_UNBOX" "PG_RESULT_BOX")
 )

;; ConnStatusType from libpq-fe.h
(define-enum CONNECTION_OK)
(define-enum CONNECTION_BAD)
(define-enum CONNECTION_STARTED)
(define-enum CONNECTION_MADE)
(define-enum CONNECTION_AWAITING_RESPONSE)
(define-enum CONNECTION_AUTH_OK)
(define-enum CONNECTION_SETENV)
(define-enum CONNECTION_SSL_STARTUP)
(define-enum CONNECTION_NEEDED)

;; ExecStatusType from libpq-fe.h
(define-enum PGRES_EMPTY_QUERY)
(define-enum PGRES_COMMAND_OK)
(define-enum PGRES_TUPLES_OK)
(define-enum PGRES_COPY_OUT)
(define-enum PGRES_COPY_IN)
(define-enum PGRES_BAD_RESPONSE)
(define-enum PGRES_NONFATAL_ERROR)
(define-enum PGRES_FATAL_ERROR)

;; Connection
(define-cproc pq-connectdb (conninfo::<const-cstring>) ::<pg-conn> PQconnectdb)
(define-cproc pq-finish (conn::<pg-conn>) ::<void>
  (unless (PGClosedP conn_scm)
    (PQfinish conn)
    (PGMarkClosed conn_scm)))
(define-cproc pq-reset (conn::<pg-conn>)  ::<void> PQreset)

(define-cproc pq-finished? (conn::<pg-conn>) ::<boolean>
  (return (PGClosedP conn_scm)))

(define-cproc pq-db (conn::<pg-conn>)   ::<const-cstring> PQdb)
(define-cproc pq-user (conn::<pg-conn>) ::<const-cstring> PQuser)
(define-cproc pq-pass (conn::<pg-conn>) ::<const-cstring> PQpass)
(define-cproc pq-host (conn::<pg-conn>) ::<const-cstring> PQhost)
(define-cproc pq-port (conn::<pg-conn>) ::<const-cstring> PQport)
(define-cproc pq-tty (conn::<pg-conn>)  ::<const-cstring> PQtty)
(define-cproc pq-options (conn::<pg-conn>) ::<const-cstring> PQoptions)
(define-cproc pq-status (conn::<pg-conn>) ::<int> PQstatus)

(define-cproc pq-error-message (conn::<pg-conn>) ::<const-cstring>
  PQerrorMessage)
(define-cproc pq-backend-pid (conn::<pg-conn>) ::<int> PQbackendPID)

;; Query
(define-cproc pq-exec (conn::<pg-conn> query::<const-cstring>) ::<pg-result>
  PQexec)

(define-cproc pq-send-query (conn::<pg-conn> command::<const-cstring>) ::<int>
  PQsendQuery)
;; pq-send-query-params
;; pq-send-prepare
;; pq-send-query-prepared
;; pq-send-describe-prepared

(define-cproc pq-get-result (conn::<pg-conn>) ::<pg-result>?
  PQgetResult)

(define-cproc pq-result-status (result::<pg-result>) ::<int> PQresultStatus)
(define-cproc pq-res-status (status::<int>) ::<const-cstring> PQresStatus)
(define-cproc pq-result-error-message (result::<pg-result>) ::<const-cstring>
  PQresultErrorMessage)

(define-cproc pq-ntuples (result::<pg-result>) ::<int> PQntuples)
(define-cproc pq-nfields (result::<pg-result>) ::<int> PQnfields)
(define-cproc pq-fname (result::<pg-result> index::<int>) ::<const-cstring>
  PQfname)
(define-cproc pq-fnumber (result::<pg-result> fname::<const-cstring>) ::<int>
  PQfnumber)
(define-cproc pq-ftype (result::<pg-result> index::<int>) ::<int> PQftype)
(define-cproc pq-fsize (result::<pg-result> index::<int>) ::<int> PQfsize)
(define-cproc pq-fmod (result::<pg-result> index::<int>) ::<int> PQfmod)
(define-cproc pq-binary-tuples (result::<pg-result>) ::<boolean> PQbinaryTuples)

(define-cproc pq-getvalue (result::<pg-result> row_id::<int> col_id::<int>)
  ;; the result may be binary, so we use explicit length.
  (let* ([val::char* (PQgetvalue result row_id col_id)]
         [len::int (PQgetlength result row_id col_id)])
    (return (Scm_MakeString val len -1 SCM_MAKSTR_COPYING))))

(define-cproc pq-getisnull (result::<pg-result> row_id::<int> col_id::<int>)
  ::<boolean> PQgetisnull)

(define-cproc pq-cmd-status (result::<pg-result>) ::<const-cstring> PQcmdStatus)
(define-cproc pq-cmd-tuples (result::<pg-result>) ::<const-cstring> PQcmdTuples)
(define-cproc pq-oid-status (result::<pg-result>) ::<const-cstring> PQoidStatus)

;; NB: The following two functions refers to result_scm, the argument
;; before unpacking PGResult* result.  Referring to the argument before
;; unwrapping is not officially supported.  There'll be a clear way to
;; do this in future versions of Gauche.
(define-cproc pq-clear (result::<pg-result>) ::<void>
  (unless (PGClosedP result_scm)
    (PQclear result)
    (PGMarkClosed result_scm)))

(define-cproc pq-cleared? (result::<pg-result>) ::<boolean>
  (return (PGClosedP result_scm)))

;; Escaping
;; NB: Postgres documentation says PQescapeString takes the "length of the
;; string" as the third arg.  As far as I see, they seem to mean "number
;; of bytes of the string", which is of course different from the length
;; of the string if we have MB string.  I wrote this assuming they meant the
;; number of bytes.  --[SK]
(define-cproc pq-escape-string (str::<string>)
  (let* ([size::ScmSmallInt]
         [from::(const char*) (Scm_GetStringContent str (& size) NULL NULL)]
         [to::char* (SCM_NEW_ATOMIC2 (.type char*) (* size 2))]
         [rsize::ScmSmallInt (PQescapeString to from size)])
    (return (Scm_MakeString to rsize -1 0))))

;; For debugging
(define-cproc pq-trace (conn::<pg-conn> oport::<output-port>) ::<void>
  (let* ([fp::FILE*]
         [fd::int (Scm_PortFileNo oport)])
    (when (< fd 0)
      (Scm_Error "pq-trace: output port must be a file port, but got %S" oport))
    (set! fp (fdopen fd "w"))
    (when (== fp NULL)
      (Scm_SysError "pq-trace: couldn't open FILE stream"))
    (PQtrace conn fp)))

(define-cproc pq-untrace (conn::<pg-conn>) ::<void> PQuntrace)

(define-cfn notice-processor (arg::void* message::(const char*)) ::void :static
  (Scm_Apply (SCM_OBJ arg) (SCM_LIST1 (SCM_MAKE_STR_COPYING message)) NULL))

(define-cproc pq-set-notice-processor (conn proc) ::<void>
  ;; We need to protect proc from being GCed, so we store it
  ;; in the foreign pointer attributes.  Because we need to access
  ;; the attributes, we can't use automatic type checking/unboxing.
  (unless (PG_CONN_P conn)
    (Scm_Error "<pg-conn> required, but got %S" conn))
  (Scm_ForeignPointerAttrSet (SCM_FOREIGN_POINTER conn)
                             'notice-processor
                             proc)
  (PQsetNoticeProcessor (PG_CONN_UNBOX conn) notice_processor proc))
