## arules specific set methods: is.superset, is.subset (only for itemMatrix)
##
setMethod("is.superset", signature(x = "itemMatrix"),
    function(x, y = NULL, proper = FALSE) {
        if (is.null(y)) 
            t(is.subset(x, proper = proper))
        else 
            t(is.subset(y, x, proper))
    }
)

setMethod("is.superset", signature(x = "associations"),
    function (x, y = NULL, proper = FALSE)
        if (is.null(y))
            is.superset(items(x), proper = proper)
        else 
            is.superset(items(x), items(y), proper)
)

## this takes about 3 times the memory but is very fast!
## I suspect internally it always uses a lot of memory.
setMethod("is.subset", signature(x = "itemMatrix"),
    function(x, y = NULL, proper = FALSE) {
        if (length(x) == 0 || (!is.null(y) && length(y) == 0))
            return(logical(0))
        if (is.null(y)) 
            m <- .Call("R_crosstab_ngCMatrix", x@data, NULL, FALSE)
        else
            m <- .Call("R_crosstab_ngCMatrix", x@data, y@data, FALSE)
        m <- m == size(x)

        ## fixme: what 
        if (proper == TRUE) 
            if (is.null(y)) 
                m <- m & outer(size(x), size(x), "<")
            else 
                m <- m & outer(size(x), size(y), "<")
        ## if we were sure that x and y contain no duplicates ...
        ## we never can be sure [ceeboo 2007]
        m
    }
)

setMethod("is.subset", signature(x = "associations"),
    function(x, y = NULL, proper = FALSE) 
        if (is.null(y)) 
            is.subset(items(x), proper = proper)
        else 
            is.subset(items(x), items(y), proper)
)

###
