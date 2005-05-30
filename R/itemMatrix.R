###*******************************************************
### dimensions

### dimensions of the binary matrix
setMethod("dim", signature(x = "itemMatrix"),
    function(x) {
    rev(dim(x@data))
    })

### number of elements (rows)
setMethod("length", signature(x = "itemMatrix"),
    function(x) {
    dim(x)[1]
    })

### produces a vector of element sizes
setMethod("size", signature(x = "itemMatrix"),
    function(x) {
    diff(x@data@p)
    })

### return item support in a set
setMethod("itemSupport", signature(x = "itemMatrix"),
    function(x, type= c("relative", "absolute")) {
      type <- match.arg(type)
      
      supports <- diff(t(x@data)@p)
      names(supports) <- itemLabels(x)

      switch(type,
	relative =  supports/length(x),
	absolute =  supports)
      })


###*******************************************************
### Coercions

setAs("matrix", "itemMatrix",
    function(from) {
    i <- t(as(from, "dgCMatrix"))
    
    ### make it binary
    i@x <- rep.int(1,length(i@x))

    new("itemMatrix", data = i,  
    	itemInfo = data.frame(labels = labels(from)[[2]]))
    })

setAs("itemMatrix", "matrix",
    function(from) {
    m <- as(t(from@data), "matrix")
    dimnames(m)[[2]] <- from@itemInfo[["labels"]]
    return(m)
    })

setAs("itemMatrix", "list",
    function(from) {
      LIST(from, decode = TRUE)
    })

setAs("list","itemMatrix", function(from, to) {
    from_names <- unique(unlist(from))
    data <- c(0:(length(from_names)-1))
    names(data) <- from_names 

    #l <- lapply(from, function(x) as(data[x], "vector"))
    l <- lapply(from, function(x) as.vector(data[x]))
    t <- new("dgTMatrix")
    j <- c(1:(length(l)))
    t@i <- unlist(l)
    t@j <- unlist(lapply(j, function(x) 
      rep.int(as.integer(x-1), length(l[[x]]))))
    t@Dim <- as.integer(c(length(data),length(l)))
    t@x <- rep.int(1, length(t@i))

    z <- as(t, "dgCMatrix")
    z@x <- rep.int(1, length(z@x)) # kill doubles
    new("itemMatrix", data=z, itemInfo = data.frame(labels = from_names))
    })

setAs("itemMatrix", "dgCMatrix",
    function(from) {
    tmp <- from@data
    dimnames(tmp)[[1]] <- from@itemInfo[["labels"]]
    return(tmp)
    })
 

###*******************************************************
### subset

### remember the sparce matrix is tored in transposed form (i <-> j)
setMethod("[", signature(x = "itemMatrix"),
    function(x, i, j, ..., drop = FALSE) {
    y <- x
    if(!missing(j)) {
      y@data <- x@data[j, i, ..., drop=drop]
      y@itemInfo <- x@itemInfo[j, , drop=FALSE]
    }else{
      y@data <- x@data[ ,i, ..., drop=drop]
    }
    return(y)
    })

setMethod("%in%", signature(x = "itemMatrix"),
    function(x, table) {
      pos <- which(apply(sapply(x@itemInfo, "%in%", table, 
        simplify = TRUE), 1, any))
      return(diff(x@data[pos]@p)==1)
    })

setMethod("decode", signature(x = "itemMatrix"),
   function(x, items) {
    
     ### missing labels
     if (is.null(x@itemInfo[["labels"]])) {
       warning("no item labels available - cannot decode item IDs!")
       return (items)
     }

     labs <- as(x@itemInfo[["labels"]], "character")
     sapply(items, function(r) labs[r], simplify=FALSE)
   })

setMethod("LIST", signature(from = "itemMatrix"),
    function(from, decode = TRUE) {
      data <- from@data
      z <- vector(length = (data@Dim[2]), mode = "list")
      l <- which(diff(data@p) != 0)
      sapply(l, function(i) 
            z[[i]] <<- as.integer(data@i[(data@p[i]+1):data@p[i+1]]+1),
            simplify=FALSE)
            # +1 since i starts with index 0 instead of 1
      if (decode == TRUE ) return(decode(from, z))
      else return(z)
   })


###*******************************************************
### accessors

setMethod("labels", signature(object = "itemMatrix"),
    function(object, ...) {
    list(items = as(object@itemInfo[["labels"]], "character"),
    	elements = paste("{",sapply(as(object, "list"),
          function(x) paste(x, collapse =", ")),"}", sep=""))
    })

setMethod("itemLabels", signature(object = "itemMatrix"),
    function(object, ...) {
    as(object@itemInfo[["labels"]], "character")
    })

setReplaceMethod("itemLabels", signature(object = "itemMatrix"),
    function(object, value) {
    object@itemInfo <- data.frame(labels = value)
    object
    })

setMethod("itemInfo", signature(object = "itemMatrix"),
    function(object) {
    object@itemInfo
    })

setReplaceMethod("itemInfo", signature(object = "itemMatrix"),
    function(object, value) {
    object@itemInfo <- value
    object
    })


###*******************************************************
### show/summary

setMethod("show", signature(object = "itemMatrix"),
    function(object) {
    cat("itemMatrix in sparse format with\n", 
      dim(object)[1],"rows (elements/transactions) and\n", 
       dim(object)[2],"columns (items)\n")
    invisible(object)
    })

setMethod("image", signature(x = "itemMatrix"),
    function(x, colorkey=FALSE, ylab="Elements (Rows)",
      xlab="Items (Columns)",...) {
    i <- t(as(x@data, "dgTMatrix"))
    image(i,colorkey=colorkey, ylab=ylab, xlab=xlab, ...)
    })


setMethod("summary", signature(object = "itemMatrix"),
    function(object, maxsum = 6, ...) {
    lst <- as(object,"list")
    chars <- as(unlist(lst), "character")
    new("summary.itemMatrix", Dim=dim(object),
      itemSummary = summary(as.factor(chars), maxsum = maxsum),
      lengths = table(size(object)),
      lengthSummary = summary(size(object)),
      itemInfo=itemInfo(object)[1:min(3, length(labels(object))),, 
      	drop = FALSE])
    })

setMethod("show", signature(object = "summary.itemMatrix"),
	function(object) {
	cat("itemMatrix in sparse format with\n",
		object@Dim[1],"rows (elements/itemsets/transactions) and\n",
		object@Dim[2],"columns (items)\n")

	cat("\nmost frequent items:\n")
	print(object@itemSummary)
	cat("\nelement (itemset/transaction) length distribution:")
	print(object@lengths)

	cat("\n")
	print(object@lengthSummary)

	if(length(names(object@itemInfo)) > 1) {
	cat("\nincludes extended item information - examples:\n")
	print(object@itemInfo)}
	})



