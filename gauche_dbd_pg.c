/*
 * gauche_dbd_pg.c
 *
 *  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
 *  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
 *  See COPYING for terms and conditions of using this software
 *
 * $Id: gauche_dbd_pg.c,v 1.2 2005/07/21 08:19:06 nel Exp $
 */

#include "gauche_dbd_pg.h"

/*
 * static function prototypes
 */

static ScmObj pq_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj pq_res_allocate(ScmClass *klass, ScmObj initargs);

/*
 * class definitions
 */

SCM_DEFINE_BUILTIN_CLASS(Scm_PqClass,
			 NULL, NULL, NULL,
			 pq_allocate,
			 NULL);

SCM_DEFINE_BUILTIN_CLASS(Scm_PqResClass,
			 NULL, NULL, NULL,
			 pq_res_allocate,
			 NULL);

/*
 * allocators
 */
static ScmObj pq_allocate(ScmClass *klass, ScmObj initargs) {
  ScmPq *h = SCM_NEW(ScmPq);
  SCM_SET_CLASS(h, SCM_CLASS_PQ);
  memset(&h->handle, 0, sizeof(h->handle));
  return SCM_OBJ(h);
}

static ScmObj pq_res_allocate(ScmClass *klass, ScmObj initargs) {
  ScmPqRes *r = SCM_NEW(ScmPqRes);
  SCM_SET_CLASS(r, SCM_CLASS_PQ_RES);
  memset(&r->res, 0, sizeof(r->res));
  return SCM_OBJ(r);
}

/*
 * cprocs
 */

ScmObj Scm_PqConnectdb(ScmString *conninfo,
		       ScmObj connection)
{
  ScmPq *c;

  if (SCM_PQ_P(connection)) {
    c = SCM_PQ(connection);
    c->handle = PQconnectdb(Scm_GetString(conninfo));
    if (c->handle == NULL) return SCM_FALSE;
  } else return SCM_FALSE;
 
  return connection;
}

ScmObj Scm_PqStatus(ScmObj connection)
{
  ScmPq *c;
  int status;
  ScmObj result = SCM_FALSE;
  if (SCM_PQ_P(connection)) {
    c = SCM_PQ(connection);
    status = PQstatus(c->handle);
    result = SCM_MAKE_INT((int)status);
  } else return SCM_FALSE;
 
  return result;
}

ScmObj Scm_PqExec(ScmString *query,
		  ScmObj connection,
		  ScmObj result)
{
  ScmPq *c;
  ScmPqRes *r;

  if (SCM_PQ_P(connection) && SCM_PQ_RES_P(result)) {
    c = SCM_PQ(connection);
    r = SCM_PQ_RES(result);
    r->res = PQexec(c->handle, Scm_GetString(query));
  } else return SCM_FALSE;

  return result;
}

ScmObj Scm_PqResultStatus(ScmObj result) {
  ExecStatusType pq_status;
  ScmObj status = SCM_FALSE;
  ScmPqRes *r;

  if (SCM_PQ_RES_P(result)) {
    r = SCM_PQ_RES(result);
    pq_status = PQresultStatus(r->res);
    status = SCM_MAKE_INT((int)pq_status);
  } else return SCM_FALSE;
  
  return status;
}

ScmObj Scm_PqResultErrorMessage(ScmObj result) {
  char *pq_error_message;
  ScmObj error_message = SCM_FALSE;
  ScmPqRes *r;

  if (SCM_PQ_RES_P(result)) {
    r = SCM_PQ_RES(result);
    pq_error_message = PQresultErrorMessage(r->res);
    error_message = SCM_MAKE_STR_COPYING(pq_error_message);
  } else return SCM_FALSE;

  return error_message;
}

ScmObj Scm_PqNtuples(ScmObj result) {
  ScmObj row_count = SCM_FALSE;
  ScmPqRes *r;
  int num_rows;

  if (SCM_PQ_RES_P(result)) {
    r = SCM_PQ_RES(result);
    num_rows = PQntuples(r->res);
    row_count = SCM_MAKE_INT(num_rows);
  } else return SCM_FALSE;

  return row_count;
}

ScmObj Scm_PqNfields(ScmObj result) {
  ScmObj column_count = SCM_FALSE;
  ScmPqRes *r;
  int num_cols;

  if (SCM_PQ_RES_P(result)) {
    r = SCM_PQ_RES(result);
    num_cols = PQnfields(r->res);
    column_count = SCM_MAKE_INT(num_cols);
  } else return SCM_FALSE;

  return column_count;
}

ScmObj Scm_Pqfname(ScmObj result, int i) {
  ScmObj sname = SCM_FALSE;
  ScmPqRes *r;
  char *name;

  if (SCM_PQ_RES_P(result)) {
    r = SCM_PQ_RES(result);
    name = PQfname(r->res, i);
    sname = SCM_MAKE_STR_COPYING (name);
  } else return SCM_FALSE;

  return sname;
}

ScmObj Scm_PqGetValue(int row_id, int col_id, ScmObj result) {
  ScmObj value = SCM_FALSE;
  ScmPqRes *r;
  char *str_value;

  if (SCM_PQ_RES_P(result)) {
    r = SCM_PQ_RES(result);
    str_value = PQgetvalue(r->res, row_id, col_id);
    value = SCM_MAKE_STR_COPYING(str_value);
  } else return SCM_FALSE;

  return value;
}

ScmObj Scm_PqFinish(ScmObj connection) {
  ScmPq *c;
  
  if (SCM_PQ_P(connection)) {
    c = SCM_PQ(connection);
    PQfinish(c->handle);
  } else return SCM_FALSE;
  
  return connection;
}

/*
 * Module initialization function.
 */
extern void Scm_Init_gauche_dbd_pglib(ScmModule*);

ScmObj Scm_Init_gauche_dbd_pg(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(gauche_dbd_pg);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("dbd.pg", TRUE));

    /* Register classes */
    Scm_InitStaticClass(&Scm_PqClass, "<pq-handle>", mod, NULL, 0);
    Scm_InitStaticClass(&Scm_PqResClass, "<pq-res>", mod, NULL, 0);
    
    /* Register stub-generated procedures */
    Scm_Init_gauche_dbd_pglib(mod);
}
