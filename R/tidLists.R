###*******************************************************
### show/summary

setMethod("show", signature(object = "tidLists"),
    function(object) {
    cat("tidLists in sparse format for\n",
      dim(object)[1],"items/itemsets (rows) and\n",
      dim(object)[2],"transactions (columns)\n")
    invisible(object)
    })

setMethod("summary", signature(object = "tidLists"),
    function(object, ...) {
    new("summary.tidLists", Dim = dim(object))
    })

setMethod("show", signature(object = "summary.tidLists"),
    function(object) {
    cat("tidLists in sparse format for\n",
      object@Dim[1],"items/itemsets (rows) and\n",
      object@Dim[2],"transactions (columns)\n")
    })


###*****************************************************
### subset

setMethod("[", signature(x = "tidLists"),
    function(x, i, j, ..., drop) {
    y <- x 
    y@data <- x@data[j,i,...,drop=drop]
    y@itemInfo = x@itemInfo[i,,drop=FALSE]
    if(!missing(j)) {
    y@transactionInfo = x@transactionInfo[j,,drop=FALSE]
    }
    return(y)	
    })



### coercions 

setAs("tidLists", "list",
    function(from) {
    LIST(from, decode = TRUE) 
    })

setMethod("LIST", signature(from = "tidLists"),
    function(from, decode = TRUE) {
    l <- LIST(as(from, "itemMatrix"), decode = FALSE)
    if(decode == TRUE) {
    l <- decode(from, l)
    names(l) <- itemLabels(from)
    }
    return(l)
    })



setAs("tidLists", "matrix",
    function(from) {
    m <- as(t(from@data), "matrix")
    if (!is.null(from@transactionInfo[["transactionIDs"]]))
    dimnames(m) <- list(from@itemInfo[["labels"]],
      from@transactionInfo[["transactionIDs"]])
    return(m)
    })

setAs("tidLists", "transactions",
    function(from) {
    new("transactions", data = t(from@data), 
      itemInfo = from@itemInfo, transactionInfo = from@transactionInfo) 
    })

setAs("transactions", "tidLists",
    function(from) {
    new("tidLists", data = t(from@data),
      itemInfo = from@itemInfo, transactionInfo = from@transactionInfo)
    })

setAs("tidLists", "dgCMatrix",
    function(from) {
    tmp <- from@data
    dimnames(tmp)[[2]] <- from@itemInfo[["labels"]]
    return(tmp)
    })

### overwrite item support from itemMatrix
### return item support in a set
setMethod("itemFrequency", signature(x = "tidLists"),
    function(x, type= c("relative", "absolute")) {
    type <- match.arg(type)

    supports <-  size(x)
    names(supports) <- itemLabels(x)

    switch(type,
      relative =  supports/dim(x)[2],
      absolute =  supports)
    })


### overwrite decode from itemMatrix
setMethod("decode", signature(x = "tidLists"),
    function(x, tids) {

### missing Transaction IDs
    if (is.null(x@transactionInfo[["transactionIDs"]])) {
    return (tids)
    }

    labs <- as(x@transactionInfo[["transactionIDs"]], "character")
    sapply(tids, function(r) labs[r], simplify=FALSE)
    })

##########################################################################
### accessors

setMethod("transactionInfo", signature(x = "tidLists"),
    function(x) {
    x@transactionInfo
    })

setMethod("labels", signature(object = "tidLists"),
    function(object, ...) {
    list(items = itemLabels(object),
      transactionIDs = as(object@transactionInfo[["transactionIDs"]],
	"character"))
    })

