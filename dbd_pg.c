/*
 * dbd_pg.c
 *
 *  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
 *  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
 *  See COPYING for terms and conditions of using this software
 *
 * $Id: dbd_pg.c,v 1.1 2005/09/02 13:12:32 shiro Exp $
 */

#include "dbd_pg.h"

/*
 * Class stuff
 */

/* Class pointers initialized by Scm_Init_dbd_pg */
ScmClass *PGConnClass;
ScmClass *PGResultClass;

static void pgconn_cleanup(ScmObj obj)
{
    PGconn *c = PG_CONN_UNBOX(obj);
    PQfinish(c);
}

static void pgresult_cleanup(ScmObj obj)
{
    PGresult *r = PG_RESULT_UNBOX(obj);
    PQclear(r);
}

/*
 * Module initialization function.
 */
extern void Scm_Init_dbd_pglib(ScmModule*);

ScmObj Scm_Init_dbd_pg(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(dbd_pg);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("dbd.pg", TRUE));

    /* Register classes */
    PGConnClass =
        Scm_MakeForeignPointerClass(mod, "<pg-conn>",
                                    NULL, pgconn_cleanup, 0);
    PGResultClass =
        Scm_MakeForeignPointerClass(mod, "<pg-result>",
                                    NULL, pgresult_cleanup, 0);
    
    /* Register stub-generated procedures */
    Scm_Init_dbd_pglib(mod);
}
