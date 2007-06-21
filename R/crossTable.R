##*******************************************************
## Cross-tabulate joint purchases across pairs of items
##

setMethod("crossTable", signature(x = "itemMatrix"),
    function(x) {
        m <- .Call("R_crosstab_ngCMatrix", x@data, NULL, TRUE)
        if (is.null(dimnames(m)))
            dimnames(m) <- list(itemLabels(x), itemLabels(x))
        m
    }
)

###
