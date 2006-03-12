## find maximal itemsets

setMethod("is.maximal", signature(x = "itemsets"),
    function(x) is.maximal(items(x)) 
)

setMethod("is.maximal", signature(x = "itemMatrix"),
    function(x) {
    .Call("R_pncount", x@data, x@data, TRUE, TRUE, FALSE) == 1
})


## old code w/o prefix tree
#setMethod("is.maximal", signature(x = "itemsets"),
    #    function(x, blocksize = 200) is.maximal(items(x), blocksize) 
    #)

#setMethod("is.maximal", signature(x = "itemMatrix"),
#    function(x, blocksize = 200) {
#        ## rowSums(is.subset(x, x, proper = TRUE)) == 0
#
#        ## for large x we use blocks now
#        ## memory requirements for is.subset (see is.super.R) 
#        ## is approx. nx^2 * (8 + 4) byte  
#
#        nx <- length(x)
#        blocksize <- floor(blocksize * 1024 * 1024 / nx / 12)
#
#        if(blocksize < 1)
#        stop("Length of x is to large. Increase usable memory blocksize!")
#
#        ## do it in one run
#        if(nx <= blocksize) 
#        return(rowSums(is.subset(x, proper = TRUE)) == 0)
#
#        ## do it in blocks
#        ismax <- logical(nx)
#
#        blockStart <- 1
#        while(blockStart < nx) {
#            cat(blockStart,"\n")
#            blockEnd <- min(blockStart+blocksize, nx)
#            ismax[blockStart:blockEnd] <- 
#            rowSums(is.subset(x[blockStart:blockEnd], x, 
#                    proper = TRUE)) == 0
#            blockStart <- blockEnd
#        }
#
#        return(ismax)
#    })
#
