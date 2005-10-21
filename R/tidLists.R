###*******************************************************
### Class tidLists
###
### transaction ID lists


###*********************************************************
### dimensions of the binary matrix
setMethod("dim", signature(x = "tidLists"),
    function(x) {
    rev(dim(x@data))
    })

### number of elements (rows)
setMethod("length", signature(x = "tidLists"),
    function(x) {
    dim(x)[1]
    })

### produces a vector of element sizes
setMethod("size", signature(x = "tidLists"),
    function(x) {
    diff(x@data@p)
    })



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

setMethod("image", signature(x = "tidLists"),
    function(x, colorkey=FALSE,
      ylab="Items/itemsets (Rows)", xlab="Transactions (Columns)",
      col.regions = gray(seq(from = 0, to = 1, length = 2)), ...) {
    i <- t(as(x@data, "dgTMatrix"))
    image(i,colorkey=colorkey, ylab=ylab, xlab=xlab,
      col.regions = col.regions, ...)
    })


### no t for associations
setMethod("t", signature(x = "tidLists"),
  function(x) {
    stop("Object not transposable! Use as() for coercion to transactions.")
  })


###*****************************************************
### subset

setMethod("[", signature(x = "tidLists", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {
      
    if(missing(j) && missing(i)) return(x)

    ### drop is always false
    drop <- FALSE

    y <- x 
    
    ### reverse i and j
    if(missing(i)) {
      y@data <- x@data[j, ..., drop=drop]
      y@transactionInfo <- x@transactionInfo[j,,drop=FALSE]
    }else if (missing(j)) {
      y@data <- x@data[,i,...,drop=drop]
      y@itemInfo = x@itemInfo[i,,drop=FALSE]
    }else{
      y@data <- x@data[j,i,...,drop=drop]
      y@itemInfo = x@itemInfo[i,,drop=FALSE]
      y@transactionInfo <- x@transactionInfo[j,,drop=FALSE]
    }

    return(y)
    })



###*****************************************************
### coercions 

setAs("tidLists", "list",
    function(from) {
    LIST(from, decode = TRUE) 
    })

setMethod("LIST", signature(from = "tidLists"),
    function(from, decode = TRUE) {
    z <- as(from@data, "list")
    if (decode == TRUE ) {
      z <- decode(z, labels(from)$transactionID)
      names(z) <- itemLabels(from)
    }
    
    return(z)
    })


setAs("tidLists", "matrix",
    function(from) {
    m <- as(t(from@data), "matrix")
    if (!is.null(from@transactionInfo[["transactionID"]]))
    dimnames(m) <- list(from@itemInfo[["labels"]],
      from@transactionInfo[["transactionID"]])
    return(m)
    })


setAs("tidLists", "dgCMatrix",
    function(from) {
    tmp <- from@data
    dimnames(tmp)[[2]] <- from@itemInfo[["labels"]]
    return(tmp)
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

setAs("tidLists", "itemMatrix",
    function(from) {
    new("transactions", data = t(from@data), 
      itemInfo = from@itemInfo) 
    })

setAs("itemMatrix", "tidLists",
    function(from) {
    new("tidLists", data = t(from@data),
      itemInfo = from@itemInfo)
    })


###*****************************************************
### accessors

setMethod("transactionInfo", signature(x = "tidLists"),
    function(x) {
    x@transactionInfo
    })

setMethod("itemInfo", signature(object = "tidLists"),
    function(object) {
     object@itemInfo
    })

setMethod("labels", signature(object = "tidLists"),
    function(object, ...) {
        transactionID <- as(object@transactionInfo[["transactionID"]],
            "character")
    if(length(transactionID) == 0) 
    transactionID <- as(1 : dim(object)[2],"character")   

    list(items = itemLabels(object),
        transactionID = transactionID) 
})

setMethod("itemLabels", signature(object = "tidLists"),
    function(object, ...) {
    as(object@itemInfo[["labels"]], "character")
    })


