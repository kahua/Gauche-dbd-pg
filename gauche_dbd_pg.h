/*
 * gauche_dbd_pg.h
 *
 *  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
 *  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
 *  See COPYING for terms and conditions of using this software
 *
 * $Id: gauche_dbd_pg.h,v 1.1 2005/07/19 00:45:42 shiro Exp $
 */

/* Prologue */
#ifndef GAUCHE_DBD_PG_H
#define GAUCHE_DBD_PG_H

#include <stdio.h>
#include <libpq-fe.h>
#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

SCM_CLASS_DECL(Scm_PqClass);
SCM_CLASS_DECL(Scm_PqResClass);

#define SCM_CLASS_PQ (&Scm_PqClass)
#define SCM_CLASS_PQ_RES (&Scm_PqResClass)

typedef struct ScmPqRec {
  SCM_HEADER;
  PGconn *handle;
} ScmPq;

typedef struct ScmPqResRec {
  SCM_HEADER;
  PGresult *res;
} ScmPqRes;

#define SCM_PQ(obj)	((ScmPq *)(obj))
#define SCM_PQ_RES(obj)	((ScmPqRes *)(obj))

#define SCM_PQ_P(obj)		(SCM_XTYPEP(obj, SCM_CLASS_PQ))
#define SCM_PQ_RES_P(obj)	(SCM_XTYPEP(obj, SCM_CLASS_PQ_RES))

extern void Scm_Init_pqlib(ScmModule *module);

extern ScmObj Scm_PqConnectdb(ScmString *conninfo,
			      ScmObj connection);

extern ScmObj Scm_PqStatus(ScmObj connection);

extern ScmObj Scm_PqExec(ScmString *query,
			 ScmObj connection,
			 ScmObj result);

extern ScmObj Scm_PqResultStatus(ScmObj result);

extern ScmObj Scm_PqResultErrorMessage(ScmObj result);

extern ScmObj Scm_PqNtuples(ScmObj result);

extern ScmObj Scm_PqNfields(ScmObj result);

extern ScmObj Scm_PqGetValue(int row_id, int col_id, ScmObj result);

extern ScmObj Scm_PqFinish(ScmObj connection);

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_DBD_PG_H */
