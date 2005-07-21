###************************************************
### accessors


setMethod("quality", signature(x = "associations"),
    function(x) {
    x@quality
    })

setReplaceMethod("quality", signature(x = "associations"),
    function(x, value) {
    x@quality <- value
    x
    })



###****************************************************
### sort + unique


setMethod("SORT", signature(x = "associations"),
   function (x, by = "support", decreasing = TRUE) {
   x[order(x@quality[[by]], decreasing = decreasing)]
   })


# this needs a working implementation of duplicated for the 
# type of associations
setMethod("unique", signature(x = "associations"),
    function(x,  incomparables = FALSE, ...) {
    x[!duplicated(x, incomparables = incomparables, ...)]
    })
	    
###************************************************
### show / summary

setMethod("show", signature(object = "associations"),
  function(object) {
  cat("set of",length(object),class(object),"\n")
  })



