##*******************************************************
## Function coverage
##
## return coverage (LHS support) for rules 

setMethod("coverage", signature(x = "rules"),
    function(x, transactions = NULL, reuse = TRUE) {
	q <- quality(x)

        if(reuse && all(c("support", "confidence") %in% names(q))) 
            return(q$support / q$confidence)
       
	if(is.null(transactions)) stop("transactions needed!")

        ## we need to calculate lhs-support    
        return(support(lhs(x), transactions))
    }
)

