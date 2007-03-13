#include <R.h>
#include <R_ext/Utils.h>
#include <Rdefines.h>

/* sparse matrix tools to ease some of the pains 
 * with package Matrix.
 *
 * Version: 0.1-2 (experimental)
 *
 * ceeboo 2006, 2007
 */

SEXP R_transpose_ngCMatrix(SEXP x) {
    if (!inherits(x, "ngCMatrix"))
	error("'x' not of class 'ngCMatrix'");
    int i, k, l, f, nr;
    SEXP r, px, ix, pr, ir;
    
    nr = INTEGER(getAttrib(x, install("Dim")))[0];
    
    px = getAttrib(x, install("p"));
    ix = getAttrib(x, install("i"));
	
    PROTECT(r = allocVector(VECSXP, 0));

    setAttrib(r, install("p"), (pr = allocVector(INTSXP, nr+1)));
    setAttrib(r, install("i"), (ir = allocVector(INTSXP, LENGTH(ix))));

    memset(INTEGER(pr), 0, sizeof(int) * (nr+1));

    for (k = 0; k < LENGTH(ix); k++)
	INTEGER(pr)[INTEGER(ix)[k]]++;
    for (k = 1; k < LENGTH(pr); k++)
	INTEGER(pr)[k] += INTEGER(pr)[k-1];
    l = LENGTH(ix)-1;
    for (i = LENGTH(px)-2; i > -1; i--) {
	f = (i) ? INTEGER(px)[i] - 1 : -1;
	for (k = l; k > f; k--)
	    INTEGER(ir)[--INTEGER(pr)[INTEGER(ix)[k]]] = i;
	l = f;
    }

    setAttrib(r, install("Dim"), (ir = allocVector(INTSXP, 2)));
    INTEGER(ir)[0] = LENGTH(px)-1;
    INTEGER(ir)[1] = nr;

    setAttrib(r, install("Dimnames"), (ir = allocVector(VECSXP, 2)));
    ix = getAttrib(x, install("Dimnames"));
    SET_VECTOR_ELT(ir, 0, VECTOR_ELT(ix, 1));
    SET_VECTOR_ELT(ir, 1, VECTOR_ELT(ix, 0));
    if (!isNull((ix = getAttrib(ix, R_NamesSymbol)))) {
	setAttrib(ir, R_NamesSymbol, (pr = allocVector(STRSXP, 2)));
	SET_STRING_ELT(pr, 0, STRING_ELT(ix, 1));
	SET_STRING_ELT(pr, 1, STRING_ELT(ix, 0));
    }

    setAttrib(r, install("factors"), allocVector(VECSXP, 0));
    setAttrib(r, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
    
    SET_S4_OBJECT(r);
    UNPROTECT(1);

    return r;
}

/* crossprod in package Matrix does not do what we 
 * want, or what we get if we use a full storage
 * representation.
 */

SEXP R_crosstab_ngCMatrix(SEXP x, SEXP y, SEXP t) {
    if (!inherits(x, "ngCMatrix"))
	error("'x' not of class 'ngCMatrix'");
    if (TYPEOF(t) != LGLSXP)
	error("'t' not of storage class logical");
    int i, j, fx, lx, fy, ly, kx, ky, ki, kj, nr, nc, s = 1;
    SEXP r, px, ix, py, iy, d1, d2, n1, n2;
  
    if (LOGICAL(t)[0] == FALSE)
	PROTECT(x = R_transpose_ngCMatrix(x));
    
    nr = nc = INTEGER(getAttrib(x, install("Dim")))[0];
    px = py = getAttrib(x, install("p"));
    ix = iy = getAttrib(x, install("i"));
    
    d1 = getAttrib(x, install("Dimnames"));
    n1 = n2 = getAttrib(d1, R_NamesSymbol);
    d1 = d2 = VECTOR_ELT(d1, 0);

    if (isNull(y))
	y = x;
    else {
	if (!inherits(y, "ngCMatrix"))
	    error("'y' not of class 'ngCMatrix'");
	if (INTEGER(getAttrib(x, install("Dim")))[0] !=
	    INTEGER(getAttrib(y, install("Dim")))[0])
	    error("the number of rows of 'x' and 'y' do not conform");
	
	if (LOGICAL(t)[0] == FALSE)
	    PROTECT(y = R_transpose_ngCMatrix(y));
	
	nc = INTEGER(getAttrib(y, install("Dim")))[0];
	py = getAttrib(y, install("p"));
	iy = getAttrib(y, install("i"));

	d2  = getAttrib(y, install("Dimnames"));
	n2 = getAttrib(d2, R_NamesSymbol);
	d2 = VECTOR_ELT(d2, 0);
	
	s  = 0;
    }

    PROTECT(r = allocMatrix(INTSXP, nr, nc));
    memset(INTEGER(r), 0, sizeof(int) * nr * nc);
    
    fx = fy = 0;
    for (i = 1; i < LENGTH(px); i++) {
	lx = INTEGER(px)[i];
	ly = (s) ? lx : INTEGER(py)[i];
	for (kx = fx; kx < lx; kx++) {
	    ki = INTEGER(ix)[kx];
	    for (ky = (s) ? kx : fy; ky < ly; ky++) {
		kj = INTEGER(iy)[ky];
		INTEGER(r)[ki+kj*nr]++;
	    }
	}
	fx = lx;
	fy = ly;
	R_CheckUserInterrupt();
    }
    if (s) {
	for (i = 0; i < nr-1; i++)
	    for (j = i+1; j < nr; j++)
		INTEGER(r)[j+i*nr] = INTEGER(r)[i+j*nr];
    }
    
    if (!isNull(d1) || !isNull(d2)) {
	setAttrib(r, R_DimNamesSymbol, (ix = allocVector(VECSXP, 2)));
	SET_VECTOR_ELT(ix, 0, d1);
	SET_VECTOR_ELT(ix, 1, d2);
	if (!isNull(n1) || !isNull(n2)) {
	    setAttrib(ix, R_NamesSymbol, (iy = allocVector(STRSXP, 2)));
	    SET_STRING_ELT(iy, 0, (isNull(n1)) ? mkChar("") : STRING_ELT(n1, 0));
	    SET_STRING_ELT(iy, 1, (isNull(n2)) ? mkChar("") : STRING_ELT(n2, 0));
	}
    }

    if (LOGICAL(t)[0] == TRUE)
	UNPROTECT(1);
    else
	UNPROTECT((s) ? 2 : 3);

    return r;
}

SEXP R_rowSums_ngCMatrix(SEXP x) {
    if (!inherits(x, "ngCMatrix"))
	error("'x' not of class 'ngCMatrix'");
    int k,  nr = INTEGER(getAttrib(x, install("Dim")))[0];
    SEXP r, ix = getAttrib(x, install("i"));

    PROTECT(r = allocVector(INTSXP, nr));
    memset(INTEGER(r), 0, sizeof(int) * nr);
    
    for (k = 0; k < LENGTH(ix); k++)
	INTEGER(r)[INTEGER(ix)[k]]++;
    
    setAttrib(r, R_NamesSymbol, VECTOR_ELT(getAttrib(x, install("Dimnames")), 0));
    UNPROTECT(1);

    return r;
}

SEXP R_colSums_ngCMatrix(SEXP x) {
    if (!inherits(x, "ngCMatrix"))
	error("'x' not of class 'ngCMatrix'");
    int k, f, l;
    SEXP r, px = getAttrib(x, install("p"));

    PROTECT(r = allocVector(INTSXP, LENGTH(px)-1));
    
    f = 0;
    for (k = 1; k < LENGTH(px); k++) {
	l = INTEGER(px)[k];
	INTEGER(r)[k-1] = l-f;
	f = l;
    }
    setAttrib(r, R_NamesSymbol, VECTOR_ELT(getAttrib(x, install("Dimnames")), 1));
    UNPROTECT(1);

    return r;
}

//

SEXP R_colSubset_ngCMatrix(SEXP x, SEXP s) {
    if (!inherits(x, "ngCMatrix"))
	error("'x' not of class 'ngCMatrix'");
    int i, j, k, n;
    SEXP r, dx, px, ix, pr, ir;
    
    dx = getAttrib(x, install("Dimnames"));
   
    r = CONS(dx, ATTRIB(x));
    SET_TAG(r, R_DimNamesSymbol);
    SET_ATTRIB(x, r);

    PROTECT(s = arraySubscript(1, s, getAttrib(x, install("Dim")), getAttrib, (STRING_ELT), x));
    
    SET_ATTRIB(x, CDR(r));

    px = getAttrib(x, install("p"));
    
    n = 0;
    for (i = 0; i < LENGTH(s); i++) {
	j = INTEGER(s)[i];
	if (j == NA_INTEGER)
	    error("invalid subscript(s)");
	n += (INTEGER(px)[j] - INTEGER(px)[j-1]);
    }

    ix = getAttrib(x, install("i"));
    
    PROTECT(r = allocVector(VECSXP, 0));
    setAttrib(r, install("p"), (pr = allocVector(INTSXP, LENGTH(s)+1)));
    setAttrib(r, install("i"), (ir = allocVector(INTSXP, n)));
    
    n = INTEGER(pr)[0] = 0;
    for (i = 0; i < LENGTH(s); i++) {
	j = INTEGER(s)[i];
	for (k = INTEGER(px)[j-1]; k < INTEGER(px)[j]; k++)
	    INTEGER(ir)[n++] = INTEGER(ix)[k];
	INTEGER(pr)[i+1] = n;
    }

    setAttrib(r, install("Dim"), (ir = allocVector(INTSXP, 2)));
    INTEGER(ir)[0] = INTEGER(getAttrib(x, install("Dim")))[0];
    INTEGER(ir)[1] = LENGTH(s);
    
    if (isNull((ix = VECTOR_ELT(dx, 1)))) 
	setAttrib(r, install("Dimnames"), dx);
    else {
	setAttrib(r, install("Dimnames"), (ir = allocVector(VECSXP, 2)));
	setAttrib(ir, R_NamesSymbol, getAttrib(dx, R_NamesSymbol));
	SET_VECTOR_ELT(ir, 0, VECTOR_ELT(dx, 0));
	SET_VECTOR_ELT(ir, 1, (pr = allocVector(STRSXP, LENGTH(s))));
	for (i = 0; i < LENGTH(s); i++) 
	    SET_STRING_ELT(pr, i, STRING_ELT(ix, INTEGER(s)[i]-1));
    }
    
    setAttrib(r, install("factors"), allocVector(VECSXP, 0));
    setAttrib(r, R_ClassSymbol, getAttrib(x, R_ClassSymbol));

    SET_S4_OBJECT(r);
    UNPROTECT(2);

    return r;
}

// R's subset functionality is a misnomer as it 
// allows many-to-many mappings. for special cases
// such as reordering of rows and one-to-one mappings 
// there exist more efficient solutions.
//
// as performing a many-to-many mapping for each
// column is inefficient we use transposition and
// column subsetting.

SEXP R_rowSubset_ngCMatrix(SEXP x, SEXP s) {
    x = R_transpose_ngCMatrix(x);
    x = R_colSubset_ngCMatrix(PROTECT(x), s);
    UNPROTECT(1);
    x = R_transpose_ngCMatrix(PROTECT(x));
    UNPROTECT(1);

    return x;
}

//
