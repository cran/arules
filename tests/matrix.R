
library(arules)

e <- new("ngCMatrix")
e

x <- new("ngCMatrix", p   = as.integer(c(0,3,5,7,7,10)),
                      i   = as.integer(c(1,2,4,1,2,0,4,0,2,5)),
                      Dim = as.integer(c(6,5)))

rownames(x) <- paste("I", 1:6, sep = "")
colnames(x) <- paste("T", 1:5, sep = "")

x

##
unclass(x)

##
getValidity(getClassDef("CsparseMatrix"))(x)
.Call("R_valid_ngCMatrix", x)

.Call("R_valid_ngCMatrix", e)

## 
.Call("R_transpose_ngCMatrix", x)
all.equal(getMethod("t", "CsparseMatrix", where = "Matrix")(x),
          .Call("R_transpose_ngCMatrix", x))

.Call("R_transpose_ngCMatrix", e)

## column subset
s <- c(1,1,3,4)
.Call("R_colSubset_ngCMatrix", x, s)
all.equal(x[,s], .Call("R_colSubset_ngCMatrix", x, s))

s <- paste("T", s, sep = "")
.Call("R_colSubset_ngCMatrix", x, s)
all.equal(x[,s], .Call("R_colSubset_ngCMatrix", x, s))

#
.Call("R_colSubset_ngCMatrix", e, integer())

## row subset
s <- c(1,1,3,4)
.Call("R_rowSubset_ngCMatrix", x, s)
all.equal(x[s,], .Call("R_rowSubset_ngCMatrix", x, s))

s <- paste("I", s, sep = "")
.Call("R_rowSubset_ngCMatrix", x, s)
all.equal(x[s,], .Call("R_rowSubset_ngCMatrix", x, s))

#
.Call("R_rowSubset_ngCMatrix", e, integer())

## reorder
.Call("R_recode_ngCMatrix", x, 6:1)

.Call("R_recode_ngCMatrix", e, integer())

## recode
.Call("R_recode_ngCMatrix", x, c(1L,3:7))

## cbind
.Call("R_cbind_ngCMatrix", e, e)

.Call("R_cbind_ngCMatrix", x, x)

## logical OR
.Call("R_or_ngCMatrix", x, x)

.Call("R_or_ngCMatrix", e, e)

## row sums
.Call("R_rowSums_ngCMatrix", x)
all.equal(rowSums(x), .Call("R_rowSums_ngCMatrix", x))

#
.Call("R_rowSums_ngCMatrix", e)

## column sums
.Call("R_colSums_ngCMatrix", x)
all.equal(colSums(x), .Call("R_colSums_ngCMatrix", x))

#
.Call("R_colSums_ngCMatrix", e)

## 
.Call("R_crosstab_ngCMatrix", x, NULL, TRUE)
.Call("R_crosstab_ngCMatrix", x, NULL, FALSE)

###
