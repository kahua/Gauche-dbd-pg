/*
 * dbd_pg.c
 *
 *  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
 *  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
 *  See COPYING for terms and conditions of using this software
 *
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
    if (!PGClosedP(obj)) {
        PGconn *c = PG_CONN_UNBOX(obj);
        PQfinish(c);
    }
}

static void pgresult_cleanup(ScmObj obj)
{
    if (!PGClosedP(obj)) {
        PGresult *r = PG_RESULT_UNBOX(obj);
        PQclear(r);
    }
}

/*
 * Open/close status management
 */
static ScmObj sym_closed;       /* symbol 'closed? */

int PGClosedP(ScmObj obj)
{
    SCM_ASSERT(SCM_FOREIGN_POINTER_P(obj));
    return !SCM_FALSEP(Scm_ForeignPointerAttrGet(SCM_FOREIGN_POINTER(obj),
                                                 sym_closed, SCM_FALSE));
}

void PGMarkClosed(ScmObj obj)
{
    SCM_ASSERT(SCM_FOREIGN_POINTER_P(obj));
    Scm_ForeignPointerAttrSet(SCM_FOREIGN_POINTER(obj),
                              sym_closed, SCM_TRUE);
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

    sym_closed = SCM_INTERN("closed?");

    /* Register stub-generated procedures */
    Scm_Init_dbd_pglib(mod);
}
