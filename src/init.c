#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _spduration_loglog_lnl(SEXP, SEXP, SEXP);
extern SEXP _spduration_sploglog_lnl(SEXP, SEXP, SEXP, SEXP);
extern SEXP _spduration_spweib_lnl(SEXP, SEXP, SEXP, SEXP);
extern SEXP _spduration_weib_lnl(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_spduration_loglog_lnl",   (DL_FUNC) &_spduration_loglog_lnl,   3},
  {"_spduration_sploglog_lnl", (DL_FUNC) &_spduration_sploglog_lnl, 4},
  {"_spduration_spweib_lnl",   (DL_FUNC) &_spduration_spweib_lnl,   4},
  {"_spduration_weib_lnl",     (DL_FUNC) &_spduration_weib_lnl,     3},
  {NULL, NULL, 0}
};

void R_init_spduration(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

