##*******************************************************
## Function coverage
##
## return coverage (LHS support) for rules 

setMethod("coverage", signature(x = "rules"),
    function(x) {
        q <- quality(x)

        if(any(!c("support", "confidence") %in% names(q)))
        stop(sQuote("x"), 
            " is missing support or confidence in its quality slot!")

        q$support / q$confidence

    }
)

