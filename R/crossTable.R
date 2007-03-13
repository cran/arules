##*******************************************************
## Cross-tabulate joint purchases across pairs of items
##

setMethod("crossTable", signature(x = "itemMatrix"),
    function(x, ...) {
        m <- as(tcrossprod(as(x, "dgCMatrix")), "matrix")

        ## FIXME:
        ## dsCMatrix -> matrix in Matrix somehow does not handle dimnames!
        dimnames(m) <- list(itemLabels(x), itemLabels(x))

        m
    })
