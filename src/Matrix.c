#include "Matrix.h"

SEXP dgC_subset(SEXP x, SEXP i, SEXP j)
{
    SEXP val, dim_R, xi_R;
    int nj = 0, nnz = 0, 
	h, k, ii, jj, ind,  maxnz, 
	*dim, *ndim, *xi, *xp, *zi, *zp;
    double *xx, *zx;
    
    /* get slots of the matrix */
    dim_R = GET_SLOT(x, install("Dim"));
    dim = INTEGER(dim_R);
    xi_R = GET_SLOT(x, install("i"));
    xi = INTEGER(xi_R);
    xp = INTEGER(GET_SLOT(x, install("p")));
    xx = REAL(GET_SLOT(x, install("x")));	    

    /* get subscript vectors */
    PROTECT(i = arraySubscript(0, i, dim_R, getAttrib, (STRING_ELT), x));
    PROTECT(j = arraySubscript(1, j, dim_R, getAttrib, (STRING_ELT), x));

    /* calculate new dimensions */
    ndim = Calloc(2, int);
    ndim[0] = LENGTH(i);
    ndim[1] = LENGTH(j);
    maxnz = ndim[0]*ndim[1];
    if(maxnz > LENGTH(xi_R)) maxnz = LENGTH(xi_R);
    
    /* allocate space for new vectors */
    zp = Calloc(ndim[1]+1, int); 
    zi = Calloc(maxnz, int); 
    zx = Calloc(maxnz, double); 
    
    /* check bounds for i */
    for (k = 0; k < ndim[0]; k++)
    {
      ii = INTEGER(i)[k];
      if (ii != NA_INTEGER) {
	if (ii < 1 || ii > dim[0]) {
	  /* clean up */
	  Free(zi); Free(zx); Free(zp);Free(ndim);
	  error("subscript i out of bounds");
	}
      }
    }
    
    /* check bounds for j and manipulate vectors */
    for (h = 0; h < ndim[1]; h++) {
      jj = INTEGER(j)[h];
      if (jj != NA_INTEGER) {
	if (jj < 1 || jj > dim[1]) {
	  /* clean up */
	  Free(zi); Free(zx); Free(zp);Free(ndim);
	  error("subscript j out of bounds");
	}
	jj--;
      }
      
      
      /* collect col pointers; first pointer is always zero */
      zp[nj] = nnz;
      
      for (ind = xp[jj]; ind < xp[jj+1]; ind++) {
	for (k = 0; k < ndim[0]; k++) {
	  ii = INTEGER(i)[k];
	  ii--;
	  if (ii == xi[ind]) {
	    zx[nnz] = xx[ind];
	    zi[nnz] = k;
	    nnz++;
	  }
	}
      }
      nj++;
    
    }
    
    /* add last col pointer; must be to last element */
    zp[nj] = nnz;
   
    /* create S4 object */
     PROTECT(val = NEW_OBJECT(MAKE_CLASS("dgCMatrix")));
    
    /* set new dimensions */
    SET_SLOT(val, install("Dim"), allocVector(INTSXP, 2)); 
    Memcpy(INTEGER(GET_SLOT(val, install("Dim"))), ndim, 2);
   
    /* set vectors */
    SET_SLOT(val, install("p"), allocVector(INTSXP, ndim[1]+1));
    Memcpy(INTEGER(GET_SLOT(val, install("p"))), zp, ndim[1]+1);
    SET_SLOT(val, install("i"), allocVector(INTSXP, nnz));
    Memcpy(INTEGER(GET_SLOT(val, install("i"))), zi, nnz);
    SET_SLOT(val, install("x"), allocVector(REALSXP, nnz));
    Memcpy(REAL(GET_SLOT(val, install("x"))), zx, nnz);
    
    /* clean up */
    Free(zi); Free(zx); Free(zp);Free(ndim);
    
    UNPROTECT(3);
    return val;
}

