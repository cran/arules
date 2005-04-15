###************************************************
### dimensions

setMethod("length", signature(x = "rules"),
   function(x) {
   length(x@lhs)
})

###***********************************************
### coercion

setAs("rules", "data.frame",
    function(from) {
    if(!length(from)) return (data.frame())
    
    if(!length(from@quality)) return(data.frame(rules = labels(from)))
      data.frame(rules = labels(from), from@quality)
    })

setMethod("labels", signature(object = "rules"),
    function(object) {
    lhs <- sapply(as(object@lhs, "list"), function(x) paste(x, collapse =", "))
    rhs <- sapply(as(object@rhs, "list"), function(x) paste(x, collapse =", "))
    paste("{",lhs,"}", " => ","{",rhs,"}", sep="")
})


###************************************************
### accessors

setMethod("itemInfo", signature(object = "rules"),
   function(object) {
   return(itemInfo(object@lhs))
})


setMethod("lhs", signature(x = "rules"),
    function(x) {
    x@lhs
    })

setReplaceMethod("lhs", signature(x = "rules"),
    function(x, value) {
    x@lhs <- value
    x
    })

setMethod("rhs", signature(x = "rules"),
    function(x) {
    x@rhs
    })

setReplaceMethod("rhs", signature(x = "rules"),
    function(x, value) {
    x@rhs <- value
    x
    })


###****************************************************
### subset, sort

setMethod("[", signature(x = "rules"),
    function(x, i, j, ..., drop)
    {
    if (!missing(j)) stop("incorrect number of dimensions")
    if (missing(i)) return(x)
    y <- x
    slots <- intersect(slotNames(x), c("lhs", "rhs"))
    for (sl in slots) slot(y, sl) <- slot(x, sl)[i]
    y@quality <- x@quality[i,,drop=FALSE]
    return(y)
    })


setMethod("subset", signature(x = "rules"),
    function(x, subset, ...) {
    if (missing(subset)) return(x)
    i <- eval(substitute(subset),c(x@quality, 
	list(lhs=x@lhs,rhs=x@rhs))) 
    x[i,]
    })



###************************************************
### summary


setMethod("summary", signature(object = "rules"), 
    function(object, ...) {
    new("summary.rules", 
       length = length(object),
       lengths = table(size(object@lhs)+size(object@rhs)),
       lengthSummary = summary(size(object@lhs)+size(object@rhs)),
       quality = summary(object@quality))
    })

setMethod("show", signature(object = "summary.rules"), 
    function(object) {
    cat("set of", object@length, "rules\n\n")
    if(object@length>0) {
      cat("rule length distribution (lhs + rhs):")
      print(object@lengths)
      cat("\n")
      print(object@lengthSummary)
      cat("\nsummary of quality measures:\n")
      print(object@quality)
    }
    })


