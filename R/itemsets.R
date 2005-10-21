###*******************************************************
### Class itemsets
###
### a set of itemsets, subclass of associations



###************************************************
### dimensions
setMethod("length", signature(x = "itemsets"),
   function(x) {
   length(x@items)
})

setMethod("size", signature(x = "itemsets"),
   function(x) {
   size(x@items)
})

###***********************************************
### coercion
setAs("itemsets", "data.frame", 
    function(from) {
    if(!length(from)) return (data.frame())
    
    if(!length(from@quality)) return(data.frame(items = labels(from)))
    return(data.frame(items = labels(from), from@quality))
    })


###***********************************************
### labels

setMethod("labels", signature(object = "itemsets"),
   function(object) {
     labels(object@items)$elements
})

setMethod("itemLabels", signature(object = "itemsets"),
    function(object) {
        itemLabels(items(object))
    })


###************************************************
### accessors


setMethod("itemInfo", signature(object = "itemsets"),
   function(object) {
   return(itemInfo(object@items))
})

setMethod("items", signature(x = "itemsets"),
    function(x) {
    x@items
    })

setReplaceMethod("items", signature(x = "itemsets"),
    function(x, value) {
    x@items <- value
    x
    })


setMethod("tidLists", signature(x = "itemsets"),
	  function(x) {
	  x@tidLists
	  })


###****************************************************
### subset, combine, duplicated, match

setMethod("[", signature(x = "itemsets", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop)
    {
    if (!missing(j)) stop("incorrect number of dimensions (j not possible)")
    if (missing(i)) return(x)
    y <- x
    slots <- intersect(slotNames(x), c("items", "tidLists"))
    for (sl in slots) slot(y, sl) <- slot(x, sl)[i]
    y@quality <- x@quality[i,,drop=FALSE]
    return(y)
    })


setMethod("c", signature(x = "itemsets"),
    function(x, ..., recursive = TRUE){

# build quality data.frame first.
# todo: merge data.frames w/differernt quality measures
    q <- x@quality
    lapply(list(...), FUN = function(i)
      q <<- rbind(q, i@quality))

# create joint itemMatrix
    z <- lapply(list(...), FUN = function(i) i@items)
    new("itemsets", items = c(x@items, z, recursive = TRUE), 
      quality = q) 
    })

setMethod("duplicated", signature(x = "itemsets"),
   function(x, incomparables = FALSE, ...) {
     duplicated(x@items, 
     	incomparables = incomparables, ...)
   })

setMethod("match", signature(x = "itemsets"),
    function(x,  table, nomatch = NA, incomparables = FALSE) {
    match(x@items, table@items,
      nomatch = nomatch, incomparables = incomparables)
    })
		  



###************************************************
### show / summary



setMethod("summary", signature(object = "itemsets"), 
    function(object, ...) {
    new("summary.itemsets", 
       length = length(object),
       items = summary(object@items,  ...),
       quality = summary(object@quality),
       tidLists = !is.null(object@tidLists))
    })

setMethod("show", signature(object = "summary.itemsets"), 
    function(object) {
    cat("set of", object@length,"itemsets\n")
    
    if(object@length>0) {     
    cat("\nmost frequent items:\n")
    print(object@items@itemSummary)
    cat("\nelement (itemset/transaction) length distribution:")
    print(object@items@lengths)

    cat("\n")
    print(object@items@lengthSummary)

    cat("\nsummary of quality measures:\n")
    print(object@quality)
    cat("\nincludes transaction ID lists:",object@tidLists,"\n")
    }
    })


