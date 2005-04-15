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
### subset, sort


setMethod("SORT", signature(x = "associations"),
   function (x, by = "support", decreasing = TRUE) {
   x[order(x@quality[[by]], decreasing = decreasing)]
   })


###************************************************
### show / summary

setMethod("show", signature(object = "associations"),
  function(object) {
  cat("set of",length(object),class(object),"\n")
  })



