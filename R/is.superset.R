## arules specific set methods: is.superset, is.subset (only for itemMatrix)

## is.subset has the code now
setMethod("is.superset", signature(x = "itemMatrix"),
    function(x, y = NULL, proper = FALSE) {
        if(is.null(y)) t(is.subset(x, proper = proper))
        else t(is.subset(y, x, proper))
    })

## this code used less memory but is very slow!
## setMethod("is.superset", signature(x = "itemMatrix", y = "itemMatrix"),
##  function(x, y, proper = FALSE) {
##    if(length(x) == 0 || length(y) == 0) return(NULL)
##    
##    ## something with crossprod on the sparce matrices would be great
##    ## but Matrix is not quite that far
##    
##    isss <- function(y.ind, x, proper = FALSE) {
##      ## select item columns
##      x.sub <- x[,y.ind]
##      ## find x's where all items in y are present
##      res <- size(x.sub) == length(y.ind)
##      if(proper) res <- res & size(x) > length(y.ind)
##      res
##    }
##
##    ## find indices for item columns in y
##    y.ind <- LIST(y, decode = FALSE)
##    res <- sapply(y.ind, isss, x = x, proper = proper)
##    #dimnames(res) <- list(labels(x), labels(y))
##    res
##  })

setMethod("is.superset", signature(x = "associations"),
    function(x, y = NULL, proper = FALSE)
    if(is.null(y)) is.superset(items(x), proper = proper)
    else is.superset(items(x), items(y), proper)
)


## subset
## this takes about 3 times the memory but is very fast!
setMethod("is.subset", signature(x = "itemMatrix"),
    function(x, y = NULL, proper = FALSE) {
        if(length(x) == 0 || (!is.null(y) && length(y) == 0)) return(logical(0))

        ## crossprod and == would be nice, but Matrix is not that far yet
        ## crossprod(x@data, y@data) == size(y)

        ## this code needs 3 times the memory of the resulting logical matrix
        if(is.null(y)) m <- as(crossprod(x@data), "matrix")
        else m <- as(t(x@data) %*% y@data, "matrix") 
        m <- m == size(x)

        if(proper == TRUE) 
        if(is.null(y)) m <- m & outer(size(x), size(x), "<")
        else m <- m & outer(size(x),size(y), "<")
        ## if we were sure that x and y contain no duplicates we could do:
        ##if(proper == TRUE) diag(m) <- FALSE

        m
    })

setMethod("is.subset", signature(x = "associations"),
    function(x, y = NULL, proper = FALSE) 
    if(is.null(y)) is.subset(items(x), proper = proper)
    else is.subset(items(x), items(y), proper)
)



