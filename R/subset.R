##*******************************************************
## Function subset
##
## subset associations using constraints on interest measures 
## or items.
##
## Note: parent.frame(2) is used so eval uses variables from the 
##       calling function (fix by ceboo)
##

##****************************************************************
## subset for itemMatrix

setMethod("subset", signature(x = "itemMatrix"),
    function(x, subset, ...) {
        if (missing(subset)) return(x)
        i <- eval(substitute(subset), list(items=x),
            parent.frame(2))
        x[i,]
    })



##****************************************************************
## subset for associations

setMethod("subset", signature(x = "itemsets"),
    function(x, subset, ...) {
        if (missing(subset)) return(x)
        i <- eval(substitute(subset), c(quality(x),
                list(items=items(x))),
            parent.frame(2))
        x[i,]
    })

setMethod("subset", signature(x = "rules"),
    function(x, subset, ...) {
        if (missing(subset)) return(x)
        
        i <- eval(substitute(subset), c(quality(x),
                list(lhs=lhs(x), rhs=rhs(x), items=items(x))),
            parent.frame(2))
        x[i,]
    })



