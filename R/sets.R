##*****************************************************************
## Basic set operations:  union, intersect, setequal, ... 
## as defined in base; worke now for all classes which implement 
## unique, match and length (in arules associations and itemMatrix). 
##


setMethod("union", signature(x = "ANY", y = "ANY"),
    function(x, y) unique(c(x, y)) 
) 

setMethod("intersect", signature(x = "ANY", y = "ANY"),
    function(x, y) unique(y[match(x, y, 0)])
)

setMethod("setequal", signature(x = "ANY", y = "ANY"),
    function(x, y) all(c(match(x, y, 0) > 0, match(y, x, 0) > 0))
)

setMethod("setdiff", signature(x = "ANY", y = "ANY"),
    function(x, y) 
    unique(if (length(x) || length(y)) x[match(x, y, 0) == 0] else x)
)

setMethod("is.element", signature(el = "ANY", set = "ANY"),
    function(el, set) match(el, set, 0) > 0 
)


## 

