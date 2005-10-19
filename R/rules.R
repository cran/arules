###*******************************************************
### Class rules
###
### a set of rules, subclass of associations


###************************************************
### dimensions
setMethod("length", signature(x = "rules"),
    function(x) {
    length(x@lhs)
    })

setMethod("size", signature(x = "rules"),
    function(x) {
    size(x@lhs) + size(x@rhs)
    })


###***********************************************
### coercion
setAs("rules", "data.frame",
    function(from) {
    if(!length(from)) return (data.frame())
    
    if(!length(from@quality)) return(data.frame(rules = labels(from)))
      data.frame(rules = labels(from), from@quality)
    })

###***********************************************
### labels

setMethod("labels", signature(object = "rules"),
    function(object) {
    paste(labels(object@lhs)$elements, " => ",
      labels(object@rhs)$elements, sep="")
})

setMethod("itemLabels", signature(object = "rules"),
    function(object) {
      itemLabels(lhs(object))
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

## get the union of rhs and lhs
setMethod("items", signature(x = "rules"),
    function(x) {
    tmp <- lhs(x)
    lhs<- as(as(tmp, "dgCMatrix"), "dgTMatrix")
    rhs<- as(as(rhs(x), "dgCMatrix"), "dgTMatrix")
## should be impossible to sum up to more than 1
    tmp@data <- as(lhs + rhs, "dgCMatrix")
    tmp
    })


###****************************************************
### subset, combine

setMethod("[", signature(x = "rules", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop)
    {
    if (!missing(j)) stop("incorrect number of dimensions (j not possible)")
    if (missing(i)) return(x)
    y <- x
    slots <- intersect(slotNames(x), c("lhs", "rhs"))
    for (sl in slots) slot(y, sl) <- slot(x, sl)[i]
    y@quality <- x@quality[i,,drop=FALSE]
    return(y)
    })


setMethod("c", signature(x = "rules"),
  function(x, ..., recursive = FALSE) {
# build quality data.frame first.
# todo: merge data.frames w/differernt quality measures
    q <- x@quality
    lapply(list(...), FUN = function(i)
      q <<- rbind(q, i@quality))

# create joint itemMatrix
    lhs <- lapply(list(...), FUN = function(i) i@lhs)
    rhs <- lapply(list(...), FUN = function(i) i@rhs)
    new("rules", lhs = c(x@lhs, lhs, recursive = TRUE),
      rhs = c(x@rhs, rhs, recursive = TRUE),
      quality = q)
  })

    
### this utility function joins the lhs and rhs so it can be
### used for duplicated, unique, etc.
 .joinedList <- function(x) {
    if (class(x) != "rules") return(NA)
    
    tmp <- LIST(x@lhs, decode = FALSE)
    rhs <- LIST(x@rhs, decode = FALSE)
    
    for (i in 1:length(x)) 
      ### -> is used to distinguish between lhs and rhs
      tmp[[i]] <- c(tmp[[i]], "->" , rhs[[i]])
      tmp
    }


setMethod("duplicated", signature(x = "rules"),
    function(x, incomparables = FALSE, ...) {
    duplicated(.joinedList(x), incomparables = incomparables, ...)
    })


setMethod("match", signature(x = "rules"),
    function(x,  table, nomatch = NA, incomparables = FALSE) {
    match(.joinedList(x), .joinedList(table),
      nomatch = nomatch, incomparables = incomparables)
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


