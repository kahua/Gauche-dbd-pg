/*
 * dbd_pg.h
 *
 *  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
 *  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
 *  See COPYING for terms and conditions of using this software
 *
 * $Id: dbd_pg.h,v 1.1 2005/09/02 13:12:32 shiro Exp $
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

extern ScmClass *PGConnClass;
#define PG_CONN_P(obj)        SCM_XTYPEP(obj, PGConnClass)
#define PG_CONN_UNBOX(obj)    SCM_FOREIGN_POINTER_REF(PGconn*, obj)
#define PG_CONN_BOX(conn)     Scm_MakeForeignPointer(PGConnClass, conn)

extern ScmClass *PGResultClass;
#define PG_RESULT_P(obj)      SCM_XTYPEP(obj, PGResultClass)
#define PG_RESULT_UNBOX(obj)  SCM_FOREIGN_POINTER_REF(PGresult*, obj)
#define PG_RESULT_BOX(res)    Scm_MakeForeignPointer(PGResultClass, res)

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_DBD_PG_H */
