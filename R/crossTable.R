##*******************************************************
## Cross-tabulate joint purchases across pairs of items
##

setMethod("crossTable", signature(x = "itemMatrix"),
    function(x, ...) {
## todo: test
## m <- .Call("R_crosstab_ngCMatrix", x, NULL, TRUE)
        m <- as(tcrossprod(as(x, "dgCMatrix")), "matrix")

        ## FIXME:
        ## dsCMatrix -> matrix in Matrix somehow does not handle dimnames!
        if (is.null(dimnames))
            dimnames(m) <- list(itemLabels(x), itemLabels(x))

        m
    })
