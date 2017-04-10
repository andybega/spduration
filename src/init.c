#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP spduration_loglog_lnl(SEXP, SEXP, SEXP);
extern SEXP spduration_sploglog_lnl(SEXP, SEXP, SEXP, SEXP);
extern SEXP spduration_spweib_lnl(SEXP, SEXP, SEXP, SEXP);
extern SEXP spduration_weib_lnl(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"spduration_loglog_lnl",   (DL_FUNC) &spduration_loglog_lnl,   3},
  {"spduration_sploglog_lnl", (DL_FUNC) &spduration_sploglog_lnl, 4},
  {"spduration_spweib_lnl",   (DL_FUNC) &spduration_spweib_lnl,   4},
  {"spduration_weib_lnl",     (DL_FUNC) &spduration_weib_lnl,     3},
  {NULL, NULL, 0}
};

void R_init_spduration(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

