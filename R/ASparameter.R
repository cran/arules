###*******************************************************
### Classes ASparameter, APparameter, ECparameter
###
### algorithm parameters for the apriori and eclat functions
### + superclass (ASparameter)

###********************************************************
### coercion


setAs("NULL", "APparameter",
function(from, to) { new(to) })

setAs("list", "APparameter", function(from, to) .list2object(from, to))

setAs("NULL", "ECparameter",
function(from, to) { new(to) })

setAs("list", "ECparameter", function(from, to) .list2object(from, to))

###**********************************************************
### initialize

setMethod("initialize", "ASparameter",
   function(.Object, minlen = 1, maxlen = 5, target = "frequent itemsets", ...) {
     if (minlen - as.integer(minlen)) stop("minlen = ", minlen, 
       " can not be coerced to integer without error.")
     if (maxlen - as.integer(maxlen)) stop("maxlen = ", maxlen, 
       " can not be coerced to integer without error.")
     .Object@minlen <- as.integer(minlen)
     .Object@maxlen <- as.integer(maxlen)
     i <- pmatch(tolower(target), .types())
     if (!is.na(i)) .Object@target <- .types()[i] else .Object@target = target
     args = list(...)
     for (i in names(args)) slot(.Object, i, check = FALSE) <- args[[i]]
     validObject(.Object)
     .Object
   })

setMethod("initialize", "APparameter",
   function(.Object, minlen = 1, maxlen = 5, target = "rules", arem = "none", ...) {
     i <- pmatch(tolower(arem), .aremtypes())
     if (!is.na(i)) .Object@arem <- .aremtypes()[i] else .Object@arem = arem
     .Object <- callNextMethod(.Object, minlen = minlen, 
       maxlen = maxlen, target = target, ...)
     .Object
   })

###********************************************************
### show

setMethod("show", signature(object = "ASparameter"),
    function(object) {
    print(data.frame(sapply(slotNames(object), 
     function(x) slot(object, x), simplify = FALSE), row.names = ""))
    invisible(object)
    })


