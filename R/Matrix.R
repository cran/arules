## provide some interfaces to our C implementations


## t in Matrix is now faster
##setMethod("t", signature(x = "ngCMatrix"),
##    function(x) .Call("R_transpose_ngCMatrix", x))

## overloading of [ for ngCMatrix cannot be accomplished
## easily as there are too many signatures to overload.


## density 
.density_Matrix <- function(x) length(x@i)/x@Dim[1]/x@Dim[2]


###
