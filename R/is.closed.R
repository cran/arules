## find closed itemsets
## Nicolas Pasquier, Yves Bastide, Rafik Taouil, and Lotfi Lakhal. (1999) 
## Discovering frequent closed itemsets for association rules. 


setMethod("is.closed", signature(x = "itemsets"),
    function(x) {

        ## An itemset X is closed if no proper super set of X is contained 
        ## in every transaction in which X is contained. Which means there 
        ## exists no super set of X with the same support count as X

        support <- quality(x)$support
        if(is.null(support)) stop(sQuote("x"), 
            " does not contain support information")

        ## Check if the set of itemsets is unique in order to
        ## avoid the problem that the same itemset could have
        ## different support counts.
        if (any(is.na(match(x, unique(x), nomatch = NA))))
            stop("itemsets not unique")

        ## since R_pnclosed only supports abs. support counts
        size <- attr(quality(x),"size.data")
        if (!is.null(size)) 
            isclosed <- .Call("R_pnclosed", x@items@data, 
                as.integer(support * size), FALSE)
        else {
            cat("legacy approach can take a while ...\n")
            
            isclosed <- logical(length(x))

            ## start with the smallest items
            for(i in 1:length(x)) {
                supersets <- is.subset(x[i], x, proper = TRUE)
                suppressWarnings(if(support[i] > max(support[supersets])) 
                    isclosed[i] <- TRUE)
            }
        }
        return(isclosed)
    })

