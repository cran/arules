##*******************************************************
## Function coverage
##
## return coverage (LHS support) for rules 

setMethod("coverage", signature(x = "rules"),
    function(x, transactions = NULL) {
        q <- quality(x)

        if(all(c("support", "confidence") %in% names(q))) 
            return(q$support / q$confidence)
        
        ## we need to calculate lhs-support    
        return(support(lhs(x), transactions))
    }
)

