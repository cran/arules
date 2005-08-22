/* ------------------------------------------------------------------------
 * Provides subset functionality directly on a sparse dgCMatrix
 * from the R-Package Matrix.
 --------------------------------------------------------------------------*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP dgC_subset(SEXP x, SEXP i, SEXP j);


