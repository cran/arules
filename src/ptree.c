#include <R.h>
#include <Rdefines.h>
#include <time.h>


// support counting and rule generation for user-supplied
// sets of itemsets. counts are stored in memory-efficient
// prefix-trees. 
//
// warning: the code is not thread-safe, but I guess so is
// most of the R source code.
//
// todo: 1) optimize insertion of left nodes. 2) check for
//	 special cases, such as all itemsets or transactions
//	 are empty, or the set of items is empty.
//	 
// Version 0.2-3
//
// (C) ceeboo 2007

#define NB_SIZE 10240			    // maximum number of items

typedef struct pnode {
    int index;
    int count;
    struct pnode *pl;
    struct pnode *pr;
} PN;

static PN *nb[NB_SIZE];			    // node pointers
static int npn, cpn, apn;		    // node counters
static int pb[NB_SIZE];			    // prefix buffer

static void pnfree(PN *p) {
    if (p == NULL)
	return;
    pnfree(p->pl);
    pnfree(p->pr);
    free(p);
    apn--;
}

static PN *pnadd(PN *p, int *x, int n) {
    if (n == 0)
	return p;
    cpn++;
    if (p == NULL) {			    // append node
	p = (PN *) malloc(sizeof(PN));
	if (p) {
	    apn++;
	    p->index = *x;
	    p->count = 0;
	    p->pr = NULL;
	    p->pl = pnadd(NULL, x+1, n-1);
	} else
	    npn = 1;
    } else
    if (p->index == *x) {		    // existing node
	p->pl = pnadd(p->pl, x+1, n-1);
    } else
    if (p->index < *x) {		    // search right subtree
	p->pr = pnadd(p->pr, x, n);
    } else {				    // prepend node
	PN *q = (PN *) malloc(sizeof(PN));
	if (q) {
	    apn++;
	    q->index = *x;
	    q->count = 0;
	    q->pr = p;
	    q->pl = pnadd(NULL, x+1, n-1);
	    p = q;
	} else
	    npn = 1;
    }
    return p;
}

// retrieve count

static int pnget(PN *p, int *x, int n) {
    if (p == NULL || n == 0)
	return 0;
    cpn++;
    if (p->index == *x) {
	npn++;
	if (n == 1)
	    return p->count;
	return pnget(p->pl, x+1, n-1);
    }
    if (p->index < *x) 
	return pnget(p->pr, x, n);
    return -1;				    // set not found
}

// count transaction

static void pncount(PN *p, int *x, int n) {
    if (p == NULL || n == 0)
	return;
    cpn++;
    if (p->index == *x) {
	npn++;
	p->count++;
	pncount(p->pl, x+1, n-1);
	pncount(p->pr, x+1, n-1);
    } else
    if (p->index < *x) 
	pncount(p->pr, x, n);
    else
	pncount(p, x+1, n-1);
}

// note that we do not drop rules with zero support
// as filtering is done later anyway. in this case, 
// confidence and lift can either be zero or NaN, 
// depending on the support of the left-hand side. 

SEXP R_pncount(SEXP R_x, SEXP R_t, SEXP R_s, SEXP R_o, SEXP R_v) {
    if (!inherits(R_x, "ngCMatrix"))
	error("'x' not of class ngCMatrix");
    if (!inherits(R_t, "ngCMatrix"))
	error("'t' not of class ngCMatrix");
    if (INTEGER(GET_SLOT(R_x, install("Dim")))[0] != 
	INTEGER(GET_SLOT(R_t, install("Dim")))[0])
	error("the number of rows of 'x' and 't' do not conform");
    if (TYPEOF(R_s) != LGLSXP)
	error("'s' not of type logical");
    if (TYPEOF(R_o) != LGLSXP)
	error("'o' not of type logical");
    if (TYPEOF(R_v) != LGLSXP)
	error("'v' not of type logical");
    int i, j, c, f, l, k, n, nr, np, ni;
    int *x, *o = NULL;
    double s, t = 0;
    SEXP px, ix, pt, it;
    SEXP r, pr, ir, pl, il, rs, rc, rl; 
#ifdef _TIME_H
    clock_t t5, t4, t3, t2, t1, t0;

    t1 = t0 = clock();
    
    if (LOGICAL(R_v)[0] == TRUE) {
	if (LOGICAL(R_o)[0] == TRUE)
	    Rprintf("reducing ... ");
	else 
	    Rprintf("preparing ... ");
    }
#endif
  
    nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
    if (nr >= NB_SIZE-1)
	error("too many items");
    
    px = GET_SLOT(R_x, install("p"));
    ix = GET_SLOT(R_x, install("i"));

    pt = GET_SLOT(R_t, install("p"));
    it = GET_SLOT(R_t, install("i"));

    if (LOGICAL(R_o)[0] == TRUE) {
	SEXP pz, iz;

	o = INTEGER(PROTECT(allocVector(INTSXP, nr)));

	memset(o, 0, sizeof(int) * nr);
	
	for (k = 0; k < LENGTH(it); k++)
	    o[INTEGER(it)[k]]++;

	memset(pb, 0, sizeof(int) * nr);
	
	for (k = 0; k < LENGTH(ix); k++)
	    pb[INTEGER(ix)[k]] = 1;

	n = c = 0;
	for (k = 0; k < nr; k++) {
	    if (pb[k])
		n += o[k];
	    else {
		o[k] = -1;
		c++;
	    }
	    pb[k] = k;
	}
	
	R_qsort_int_I(o, pb, 1, nr);

	for (k = 0; k < nr; k++)
	    o[pb[k]] = (k < c) ? -1 : k;
	
	PROTECT(iz = allocVector(INTSXP, LENGTH(ix)));
	f = 0;
	for (i = 1; i < LENGTH(px); i++) {
	    l = INTEGER(px)[i];
	    if (f == l)
		continue;
	    for (k = f; k < l; k++)
		INTEGER(iz)[k] = o[INTEGER(ix)[k]];
	    R_isort(INTEGER(iz)+f, l-f);
	    f = l;
	}
	ix = iz;
	
	PROTECT(pz = allocVector(INTSXP, LENGTH(pt)));
	PROTECT(iz = allocVector(INTSXP, n));
	
	f = n = INTEGER(pz)[0] = 0;
	for (i = 1; i < LENGTH(pt); i++) {
	    l = INTEGER(pt)[i];
	    if (f < l) {
		for (k = f, f = n; k < l; k++)
		    if ((j = o[INTEGER(it)[k]]) > -1)
			INTEGER(iz)[n++] = j;
		R_isort(INTEGER(iz)+f, n-f);
		f = l;
	    }
	    INTEGER(pz)[i] = n;
	}
	pt = pz;
	ni = LENGTH(it);
	it = iz;
	
	if (LOGICAL(R_s)[0] == FALSE)
	    memcpy(o, pb, sizeof(int) * nr);

#ifdef _TIME_H
	t1 = clock();
	if (LOGICAL(R_v)[0] == TRUE) {
	    Rprintf("%i indexes, dropped %i (%.2f) items [%.2fs]\n", 
		    LENGTH(ix) + ni, c, 1 - (double) n / ni,
		    ((double) t1 - t0) / CLOCKS_PER_SEC);
	    Rprintf("preparing ... ");
	}
#endif
    }
    
    cpn = apn = npn = 0;

    k = nr;
    nb[k] = NULL;
    while (k-- > 0)
	nb[k] = pnadd(nb[k+1], &k, 1);

    if (npn) {
	pnfree(*nb);
	error("node allocation failed");
    }
    
    np = ni = 0;
    
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0)
	    continue;
	x = INTEGER(ix)+f;
	pnadd(nb[*x], x, n);
	if (LOGICAL(R_s)[0] == FALSE && n > 1) {
	    if (n > 2) {
		memcpy(pb, x, sizeof(int) * n);
		for (k = 0; k < n-1; k++) {
		    if (k > 0) {
			j     = pb[0];
			pb[0] = pb[k];
			pb[k] = j;
		    }
		    pnadd(nb[pb[1]], pb+1, n-1);
		}
	    }
	    np += n;
	    ni += n * (n-1);
	}
	if (npn) {
	    pnfree(*nb);
	    error("node allocation failed");
	}
	f = l;
	R_CheckUserInterrupt();
    }

#ifdef _TIME_H
    t2 = clock();
    if (LOGICAL(R_v)[0] == TRUE) {
	Rprintf("%i itemsets, created %i (%.2f) nodes [%.2fs]\n",
		2 * np +  LENGTH(px) - 1, apn, (double) apn / cpn,
		((double) t2 - t1) / CLOCKS_PER_SEC);
	Rprintf("counting ... ");
    }
#endif
    
    cpn = npn = 0;

    f = 0;
    for (i = 1; i < LENGTH(pt); i++) {
	l = INTEGER(pt)[i];
	n = l-f;
	if (n == 0)
	    continue;
	x = INTEGER(it)+f;
	pncount(nb[*x], x, n);
	f = l;
	R_CheckUserInterrupt();
    }

#ifdef _TIME_H
    t3 = clock();
    if (LOGICAL(R_v)[0] == TRUE) {
	Rprintf("%i transactions, processed %i (%.2f) nodes [%.2fs]\n",
		LENGTH(pt) - 1, cpn, (double) npn / cpn, 
		((double) t3 - t2) / CLOCKS_PER_SEC);
	Rprintf("writing ... ");
    }
#endif
   
    if (LOGICAL(R_s)[0] == TRUE) {
	PROTECT(r = allocVector(INTSXP, LENGTH(px)-1));
	// warnings
	pl = il = pr = ir = rs = rc = rl = (SEXP)0;
    } else {
	SEXP o, p;
	PROTECT(r = allocVector(VECSXP, 5));
	
	SET_VECTOR_ELT(r, 0, (o = NEW_OBJECT(MAKE_CLASS("ngCMatrix"))));
	SET_SLOT(o, install("p"),   (pl = allocVector(INTSXP, np+1)));
	SET_SLOT(o, install("i"),   (il = allocVector(INTSXP, ni)));
	SET_SLOT(o, install("Dim"), (p  = allocVector(INTSXP, 2)));
	INTEGER(p)[0] = nr;
	INTEGER(p)[1] = np;

	SET_VECTOR_ELT(r, 1, (o = NEW_OBJECT(MAKE_CLASS("ngCMatrix"))));
	SET_SLOT(o, install("p"),   (pr = allocVector(INTSXP, np+1)));
	SET_SLOT(o, install("i"),   (ir = allocVector(INTSXP, np)));
	SET_SLOT(o, install("Dim"), (p  = allocVector(INTSXP, 2)));
	INTEGER(p)[0] = nr;
	INTEGER(p)[1] = np;
	
	SET_VECTOR_ELT(r, 2, (rs = allocVector(REALSXP, np)));
	SET_VECTOR_ELT(r, 3, (rc = allocVector(REALSXP, np)));
	SET_VECTOR_ELT(r, 4, (rl = allocVector(REALSXP, np)));
	
	INTEGER(pl)[0] = INTEGER(pr)[0] = np = ni = 0;

	t = (double) LENGTH(pt)-1;
    }

    cpn = npn = 0;
    
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) {
	    if (LOGICAL(R_s)[0] == TRUE)
		INTEGER(r)[i-1] = 0;
	    continue;
	}
	x = INTEGER(ix)+f;
	c = pnget(nb[*x], x, n);
	if (LOGICAL(R_s)[0] == TRUE)
	    INTEGER(r)[i-1] = c;
	else if (n > 1) {
	    s = c / t;
	    memcpy(pb, x, sizeof(int) * n);
	    for (k = 0; k < n; k++) {
		if (k > 0) {
		    j     = pb[0];
		    pb[0] = pb[k];
		    pb[k] = j;
		}
		REAL(rs)[np] = s;
		REAL(rc)[np] = c / (double)   pnget(nb[pb[1]], pb+1, n-1);
		REAL(rl)[np] = REAL(rc)[np] / pnget(nb[pb[0]], pb, 1) * t;
		
		INTEGER(ir)[np++] = pb[0];
		INTEGER(pr)[np]   = np;
		for (j = 1; j < n; j++)
		    INTEGER(il)[ni++] = pb[j];
		INTEGER(pl)[np] = ni;
	    }
	}
	f = l;
	R_CheckUserInterrupt();
    }
  
    pnfree(*nb);

    if (apn)
	error("node deallocation imbalance %i", apn);
    
#ifdef _TIME_H
    t4 = clock();

    if (LOGICAL(R_v)[0] == TRUE) {
	if (LOGICAL(R_s)[0] == FALSE)
	    Rprintf("%i rules, ", np);
	else
	    Rprintf("%i counts, ", LENGTH(px)-1);
	Rprintf("processed %i (%.2f) nodes [%.2fs]\n", cpn, (double) npn / cpn,
		((double) t4 - t3) / CLOCKS_PER_SEC);
    }
#endif

    if (LOGICAL(R_o)[0] == TRUE) {
	if (LOGICAL(R_s)[0] == FALSE) {
#ifdef _TIME_H
	    if (LOGICAL(R_v)[0] == TRUE)
		Rprintf("recoding ... ");
#endif	
	    f = 0;
	    for (i = 1; i < LENGTH(pl); i++) {
		l = INTEGER(pl)[i];
		if (f == l)
		    continue;
		for (k = f; k < l; k++)
		    INTEGER(il)[k] = o[INTEGER(il)[k]];
		R_isort(INTEGER(il)+f, l-f);
		f = l;
	    }
	    for (k = 0; k < LENGTH(ir); k++)
		INTEGER(ir)[k] = o[INTEGER(ir)[k]];

#ifdef _TIME_H
	    t5 = clock();
	    if (LOGICAL(R_v)[0] == TRUE)
		Rprintf(" %i indexes [%.2fs]\n", LENGTH(il) + LENGTH(ir), 
			((double) t5 - t4) / CLOCKS_PER_SEC);
#endif
	}
	UNPROTECT(5);
    }
    else
	UNPROTECT(1);

    return r;
}

// index itemsets

static void pnindex(PN *p) {
    if (p == NULL)
	return;
    p->count = cpn++;
    pnindex(p->pl);
    pnindex(p->pr);
}

SEXP R_pnindex(SEXP R_x, SEXP R_v) {
    if (!inherits(R_x, "ngCMatrix"))
	error("'x' not of class ngCMatrix");
    if (TYPEOF(R_v) != LGLSXP)
	error("'v' not of type logical");
    int i, k, f, l, n, nr;
    int *x;
    SEXP r, px, ix;
    
#ifdef _TIME_H
    clock_t t2, t1 = clock();

    if (LOGICAL(R_v)[0] == TRUE)
	Rprintf("indexing ... ");
#endif
    nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
    if (nr >= NB_SIZE-1)
	error("too many items");
    
    px = GET_SLOT(R_x, install("p"));
    ix = GET_SLOT(R_x, install("i"));

    cpn = apn = npn = 0;
    
    k = nr;
    nb[k] = NULL;
    while (k-- > 0)
	nb[k] = pnadd(nb[k+1], &k, 1);

    if (npn) {
	pnfree(*nb);
	error("node allocation failed");
    }
   
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) 
	    continue;
	x = INTEGER(ix)+f;
	pnadd(nb[*x], x, n);
	if (npn) {
	    pnfree(*nb);
	    error("node allocation failed");
	}
	f = l;
	R_CheckUserInterrupt();
    }

    PROTECT(r = allocVector(INTSXP, LENGTH(px)-1));
  
    cpn = 1;
    
    pnindex(*nb);

    cpn = npn = 0;
   
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) {
	    INTEGER(r)[i-1] = 0;
	    continue;
	}
	x = INTEGER(ix)+f;
	INTEGER(r)[i-1] = pnget(nb[*x], x, n);
	f = l;
	R_CheckUserInterrupt();
    }

    pnfree(*nb);

    if (apn)
	error("node deallocation imbalance %i", apn);
#ifdef _TIME_H
    t2 = clock();
    if (LOGICAL(R_v)[0] == TRUE)
	Rprintf(" %i itemsets [%.2fs]\n", LENGTH(px) - 1, 
		((double) t2-t1) / CLOCKS_PER_SEC);
#endif
    
    UNPROTECT(1);
    
    return r;
}

// update all subsets to the maximum count
// of a superset
//
// fixme: generalize to different uses

static int pnc;

static void pnsmax(PN *p, int *x, int n, int l) {
    if (p == NULL || n == 0)
	return;
    cpn++;
    if (p->index == *x) {
	npn++;
	if ((l > n || n > 1) && p->count < pnc)
	    p->count = pnc;
	pnsmax(p->pl, x+1, n-1, l-1);
	pnsmax(p->pr, x+1, n-1, l);
    } else
    if (p->index < *x) 
	pnsmax(p->pr, x, n, l);
    else
	pnsmax(p, x+1, n-1, l);
}

SEXP R_pnclosed(SEXP R_x, SEXP R_c, SEXP R_v) {
    if (!inherits(R_x, "ngCMatrix"))
	error("'x' not of class ngCMatrix");
    if (TYPEOF(R_c) != INTSXP)
	error("'x' not of storage type integer");
    if (LENGTH(R_c) != INTEGER(GET_SLOT(R_x, install("Dim")))[1])
	error("'x' and 'c' not the same length");
    if (TYPEOF(R_v) != LGLSXP)
	error("'v' not of type logical");
    int i, k, f, l, n, nr;
    int *x;
    SEXP r, px, ix;
    
#ifdef _TIME_H
    clock_t t2, t1 = clock();

    if (LOGICAL(R_v)[0] == TRUE) 
	Rprintf("checking ... ");
#endif
    nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
    if (nr >= NB_SIZE-1)
	error("too many items");
    
    px = GET_SLOT(R_x, install("p"));
    ix = GET_SLOT(R_x, install("i"));

    cpn = apn = npn = 0;
    
    k = nr;
    nb[k] = NULL;
    while (k-- > 0)
	nb[k] = pnadd(nb[k+1], &k, 1);

    if (npn) {
	pnfree(*nb);
	error("node allocation failed");
    }
   
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) 
	    continue;
	x = INTEGER(ix)+f;
	pnadd(nb[*x], x, n);
	if (npn) {
	    pnfree(*nb);
	    error("node allocation failed");
	}
	f = l;
	R_CheckUserInterrupt();
    }

    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) 
	    continue;
	x   = INTEGER(ix)+f;
	pnc = INTEGER(R_c)[i-1];
	pnsmax(nb[*x], x, n, n);
	f = l;
	R_CheckUserInterrupt();
    }

    PROTECT(r = allocVector(LGLSXP, LENGTH(px)-1));
  
    cpn = npn = 0;
   
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
	l = INTEGER(px)[i];
	n = l-f;
	if (n == 0) {
	    LOGICAL(r)[i-1] = FALSE;
	    continue;
	}
	x = INTEGER(ix)+f;
	k = pnget(nb[*x], x, n);
	LOGICAL(r)[i-1] = (INTEGER(R_c)[i-1] > k) ? TRUE : FALSE;
	f = l;
	R_CheckUserInterrupt();
    }

    pnfree(*nb);

    if (apn)
	error("node deallocation imbalance %i", apn);
#ifdef _TIME_H
    t2 = clock();
    if (LOGICAL(R_v)[0] == TRUE)
	Rprintf(" %i itemsets [%.2fs]\n", LENGTH(px) - 1, 
		((double) t2-t1) / CLOCKS_PER_SEC);
#endif
    
    UNPROTECT(1);
    
    return r;
}

//
