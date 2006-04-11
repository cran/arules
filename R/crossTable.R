###*******************************************************
### Cross-tabulate joint purchases across pairs of items
###

setMethod("crossTable", signature(x = "itemMatrix"),
  function(x, ...) {
    #crossprod(as(x, "matrix"))
    
    # much faster
    m <- as(tcrossprod(as(x, "dgCMatrix")),"matrix")
    
    # dgCMatrix somehow does not handle dimnames well!
    dimnames(m) <- list(itemLabels(x), itemLabels(x))
    
    m
    
  })
