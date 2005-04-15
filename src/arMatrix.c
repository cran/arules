#include "arMatrix.h"

SEXP csc_subset(SEXP x, SEXP i, SEXP j)
{
    SEXP val, dim;
    int nj = 0, nnz = 0, 
	    h, k, ii, jj, ind,  maxnz, 
	    *dims, *ndims, *xi, *xp, *zi, *zp;
    double *xx, *zx;
    
    PROTECT(val = NEW_OBJECT(MAKE_CLASS("dgCMatrix")));
    dims = INTEGER(GET_SLOT(x, install("Dim")));
    xi = INTEGER(GET_SLOT(x, install("i")));
    xp = INTEGER(GET_SLOT(x, install("p")));
    xx = REAL(GET_SLOT(x, install("x")));	    

    dim = GET_SLOT(x, install("Dim"));
    i = arraySubscript(0, i, dim, getAttrib, (STRING_ELT), x);
    j = arraySubscript(1, j, dim, getAttrib, (STRING_ELT), x);

    ndims = Calloc(2, int);
    ndims[0] = LENGTH(i);
    ndims[1] = LENGTH(j);
    maxnz = ndims[0]*ndims[1];
    SET_SLOT(val, install("Dim"), allocVector(INTSXP, 2));
    Memcpy(INTEGER(GET_SLOT(val, install("Dim"))), ndims, 2);
    zi = Calloc(maxnz, int); zx = Calloc(maxnz, double);
    zp = Calloc(ndims[1]+1, int); 
    for (k = 0; k < ndims[0]; k++)
    {
	    ii = INTEGER(i)[k];
	    if (ii != NA_INTEGER) {
		    if (ii < 1 || ii > dims[0])
			    error("subscript out of bounds");
	    }
    }
    for (h = 0; h < ndims[1]; h++) {
	    jj = INTEGER(j)[h];
	    if (jj != NA_INTEGER) {
		    if (jj < 1 || jj > dims[1])
			    error("subscript out of bounds");
		    jj--;
	    }
	    zp[nj] = nnz;
	    nj++;
	    for (ind = xp[jj]; ind < xp[jj+1]; ind++) {
		    for (k = 0; k < ndims[0]; k++) {
			    ii = INTEGER(i)[k];
			    ii--;
			    if (ii == xi[ind]) {
				    zx[nnz] = xx[ind];
				    zi[nnz] = k;
				    nnz++;
			    }
		    }
	    }
    }
    zp[nj] = nnz;
    nj++;
    SET_SLOT(val, install("p"), allocVector(INTSXP, nj));
    Memcpy(INTEGER(GET_SLOT(val, install("p"))), zp, nj);
    SET_SLOT(val, install("i"), allocVector(INTSXP, nnz));
    Memcpy(INTEGER(GET_SLOT(val, install("i"))), zi, nnz);
    SET_SLOT(val, install("x"), allocVector(REALSXP, nnz));
    Memcpy(REAL(GET_SLOT(val, install("x"))), zx, nnz);
    Free(zi); Free(zx); Free(zp);Free(ndims);
    UNPROTECT(1);
    return val;
}

SEXP arMatrix_var(SEXP x, SEXP i)
{
    SEXP val, attrib;
    int *xi, *xp, *lev, *plev, anz, *ndims, k, ind, *ans, id, new;
    double *xx;

    id = *INTEGER(i);
    xi = INTEGER(GET_SLOT(x, install("i")));
    xp = INTEGER(GET_SLOT(x, install("p")));
    xx = REAL(GET_SLOT(x, install("x")));
    attrib = GET_SLOT(x, install("attributes"));
    lev = INTEGER(GET_SLOT(attrib, install("assign")));
    anz = LENGTH(GET_SLOT(attrib, install("assign")));
    plev = Calloc(anz+1, int);
    plev[0] = 0;
    for (k = 1; k < anz + 1; k++) {
	    plev[k] = plev[k-1] + lev[k-1];
    }
    ndims = INTEGER(GET_SLOT(x, install("Dim")));
    ans = Calloc(ndims[1], int);
    new = 1;
    for (k = 0; k < ndims[1]; k++) {
	    for (ind = xp[k]; ind < xp[k+1]; ind++) {
		    if ((plev[(id-1)] <= xi[ind]) & (xi[ind] < plev[id])) {
			    if (new) {
				    ans[k] = xi[ind];
				    new = 0;
			    }
			    else error("The same variable is specified twice.");
		    }
	    }
	    if (new) {
		    ans[k] = NA_REAL;
	    }
	    else new = 1;
    }
    val = PROTECT(allocVector(INTSXP, ndims[1]));
    Memcpy(INTEGER(val), ans, ndims[1]);
    UNPROTECT(1);
    return val;
}
