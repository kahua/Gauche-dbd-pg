/*
 * dbd_pg.h
 *
 *  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
 *  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
 *  See COPYING for terms and conditions of using this software
 *
 */

/* Prologue */
#ifndef GAUCHE_DBD_PG_H
#define GAUCHE_DBD_PG_H

#include <stdio.h>
#include <libpq-fe.h>
#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

/*
 * Internal Classes
 */

/* We use foreign pointer to hold PG connection and result set
   handles.  Once a handle is closed, we mark the foreign pointer
   so by setting a foreign-pointer-attribute 'closed to #t. */

extern ScmClass *PGConnClass;
#define PG_CONN_P(obj)        SCM_XTYPEP(obj, PGConnClass)
#define PG_CONN_UNBOX(obj)    SCM_FOREIGN_POINTER_REF(PGconn*, obj)
#define PG_CONN_BOX(conn)     Scm_MakeForeignPointer(PGConnClass, conn)

extern ScmClass *PGResultClass;
#define PG_RESULT_P(obj)      SCM_XTYPEP(obj, PGResultClass)
#define PG_RESULT_UNBOX(obj)  SCM_FOREIGN_POINTER_REF(PGresult*, obj)
#define PG_RESULT_BOX(res)    Scm_MakeForeignPointer(PGResultClass, res)

extern int  PGClosedP(ScmObj obj);
extern void PGMarkClosed(ScmObj obj);

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_DBD_PG_H */
