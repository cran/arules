## provide some interfaces to our C implementations

setMethod("t", signature(x = "ngCMatrix"),
    function(x) .Call("R_transpose_ngCMatrix", x))

## FIXME crossprod should be here too, but see below.

## overloading of [ for ngCMatrix cannot be accomplished
## easily as there are too many signatures to overload.

###
