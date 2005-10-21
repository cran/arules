###*******************************************************
### Virtual class associations
###
### vitual class which defines the (and implements some) common 
### functionality for associations (e.g., rules, itemsets)


###************************************************
### common methods

### accessors for quality
setMethod("quality", signature(x = "associations"),
  function(x) {
    x@quality
  })

setReplaceMethod("quality", signature(x = "associations"),
  function(x, value) {
    x@quality <- value
    x
  })


### sort + unique
setMethod("SORT", signature(x = "associations"),
  function (x, by = "support", decreasing = TRUE) {
    if(length(x) == 0) return(x)
    x[order(x@quality[[by]], decreasing = decreasing)]
  
  })


# this needs a working implementation of duplicated for the 
# type of associations
setMethod("unique", signature(x = "associations"),
  function(x,  incomparables = FALSE, ...) {
    x[!duplicated(x, incomparables = incomparables, ...)]
  })

### show
setMethod("show", signature(object = "associations"),
  function(object) {
    cat("set of",length(object),class(object),"\n")
  })

### no t for associations
setMethod("t", signature(x = "associations"),
  function(x) {
    stop("Object not transposable!")  
})


###************************************************
### implementations of associations must provide minimal interface

setMethod("items", signature(x = "associations"),
  function(x) {
    stop(paste("Method items not implemented for class", class(x),"\n"))
  })

setMethod("length", signature(x = "associations"),
  function(x) {
    stop(paste("Method length not implemented for class", class(x),"\n"))
  })

setMethod("labels", signature(object = "associations"),
  function(object) {
    stop(paste("Method duplicated not implemented for class", class(object),"\n"))
  })


###****************************************************
### writing associations to disk

		   
setMethod("WRITE", signature(x = "associations"),
  function(x, ...) {
    write.table(as(x, "data.frame"), ...)
  })


