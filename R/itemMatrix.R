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


setMethod("LIST", signature(from = "itemMatrix"),
    function(from, decode = TRUE) {
      z <- as(from@data, "list")
      if (decode == TRUE ) return(decode(z, itemLabels(from)))
      else return(z)
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
### match: find elements which contain some items (as 
###        labels or in itemInfo)
setMethod("%in%", signature(x = "itemMatrix"),
    function(x, table) {
    pos <- which(apply(sapply(x@itemInfo, "%in%", table, 
        simplify = TRUE), 1, any))
      if(length(pos) == 0) pos <- 0 
      return(diff(x@data[pos]@p)==1)
    })


###*******************************************************
### subset, combine, duplicated, unique 

### remember the sparce matrix is tored in transposed form (i <-> j)
setMethod("[", signature(x = "itemMatrix", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {
    
    if(missing(j) && missing(i)) return(x)
    
    ### drop is always false
    drop <- FALSE 
    
    y <- x
    
    ### i and j are reversed!
    if (missing(j)) {
        y@data <- x@data[ ,i, ..., drop=drop]
        return(y)
    }
        
    ### transalate item labels to column numbers, if j is character
    if (is.character(j)) 
      j <- sort(unique(unlist(sapply(j, 
        function(e) grep(e, as.character(itemInfo(x)$labels))))))
    
    if(missing(i)) {
        y@data <- x@data[j, ..., drop=drop]
        y@itemInfo <- x@itemInfo[j, , drop=FALSE]
        return(y)
    }

    ### so we have i and j
    y@data <- x@data[j, i, ..., drop=drop]
    y@itemInfo <- x@itemInfo[j, , drop=FALSE]
    return(y)
    
})



setMethod("combine", signature(first = "itemMatrix"),
    function(first, ..., as_list = NULL){
     
     ### in case we allready get a list
     if(!is.null(as_list)) z <- as_list else z <- list(...)
     
     if(length(z) < 1) return(first) 
   
     num_items <- first@data@Dim[1]
     num_trans <- first@data@Dim[2]
     i <- first@data@i
     p <- first@data@p
     x <- first@data@x
     ### pmax makes sure that the column counts for the added elements
     # start with the number of the previous element
     pmax <- p
    
     for (elem in z) {
        if(elem@data@Dim[1] != num_items) 
	   stop ("Number of items mismatch")
	pmax <-  p[length(p)]
	
	i <- c(i, elem@data@i)
	p <- c(p, (elem@data@p[-1] + pmax))
        x <- c(x, elem@data@x)
	num_trans <- num_trans + elem@data@Dim[2]
     }
     
     data <- new("dgCMatrix", i = i, p = p, x = x, 
       Dim = c(num_items,num_trans) )
     new("itemMatrix", data = data, 
       itemInfo = z[[1]]@itemInfo)
   })

setMethod("duplicated", signature(x = "itemMatrix"),
    function(x, incomparables = FALSE, ...) {
    duplicated(LIST(x, decode = FALSE),
      incomparables = incomparables, ...)
    })

setMethod("unique", signature(x = "itemMatrix"),
    function(x,  incomparables = FALSE, ...) {
    x[!duplicated(x, incomparables = incomparables, ...)]
    })

setMethod("match", signature(x = "itemMatrix"),
    function(x,  table, nomatch = NA, incomparables = FALSE) {
    match(LIST(x, decode = FALSE), LIST(table, decode = FALSE), 
      nomatch = nomatch, incomparables = incomparables)
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
### show/summary + plots

setMethod("show", signature(object = "itemMatrix"),
    function(object) {
    cat("itemMatrix in sparse format with\n", 
      dim(object)[1],"rows (elements/transactions) and\n", 
       dim(object)[2],"columns (items)\n")
    invisible(object)
    })


setMethod("image", signature(x = "itemMatrix"),
    function(x, colorkey=FALSE, 
      ylab="Elements (Rows)", xlab="Items (Columns)", 
      col.regions = gray(seq(from = 0, to = 1, length = 2)), ...) {
    i <- t(as(x@data, "dgTMatrix"))
    image(i,colorkey=colorkey, ylab=ylab, xlab=xlab, 
      col.regions = col.regions, ...)
    })


setMethod("itemFrequencyPlot", signature(x = "itemMatrix"),
    function(x, type = c("relative", "absolute"),  
      population = NULL,
      cex.names =  par("cex.axis"), ...) {
      type <- match.arg(type)
     
      itemFrequency <- itemFrequency(x, type)
     
      # make enough space for item labels
      maxLabel <- max(strwidth(names(itemFrequency), units = "inches", 
      	cex = cex.names))
      mai <- par("mai")
      par(mai = c(maxLabel+0.5, mai[-1]))
     
      midpoints <- barplot(itemFrequency, 
        las = 2, cex.name = cex.names,
	ylab = paste("item frequency (", type, ")", sep = ""), 
	...)
      
      # add population means
      if(!is.null(population))
      lines(midpoints, itemFrequency(population))
      
      # reset image margins
      par(mai = mai)
      
      # return mitpoints
      invisible(midpoints)
      })


setMethod("itemFrequencyPlot", signature(x = "itemMatrix"),
    function(x, type = c("relative", "absolute"),  
      population = NULL, deviation = FALSE, horiz = FALSE,
      cex.names =  par("cex.axis"), xlab = NULL, ylab = NULL, ...) {
      
      type <- match.arg(type)
      
      # force relative for deviation
      if(deviation == TRUE) type <- "relative"
    
      # get frequencies
      itemFrequency <- itemFrequency(x, type)
      if(!is.null(population))
      	population.itemFrequency <- itemFrequency(population, type)

      # regular plot
      if(deviation == FALSE) {
          label <- paste("item frequency (", type, ")", sep="")
      }else{

          # show relative deviations instead of frequencies
	  if(is.null(population)) 
	  	stop("population needed for plotting deviations!")
	  itemFrequency <- (itemFrequency - population.itemFrequency) / 
	     population.itemFrequency
          label <- paste("relative deviation from population", sep="")
      }

    
      # make enough space for item labels
      maxLabel <- max(strwidth(names(itemFrequency), units = "inches", 
      	cex = cex.names))
      op.mai <- par("mai")
      if (horiz == FALSE) {
      	par(mai = c(maxLabel+0.5, op.mai[-1]))
        if(is.null(ylab)) ylab <- label	
      }else{
        par(mai = c(op.mai[1], maxLabel+0.5, op.mai[-c(1,2)]))
        if(is.null(xlab)) xlab <- label
      }
      
      midpoints <- barplot(itemFrequency, 
        las = 2, cex.name = cex.names, horiz = horiz,
	xlab = xlab, ylab = ylab, ...)
      
      # add population means
      if(!is.null(population) && deviation == FALSE)
        if(horiz == FALSE) lines(midpoints, population.itemFrequency)
        else lines(population.itemFrequency, midpoints)
      
      
      # reset image margins
      par(mai = op.mai)
      
      # return mitpoints
      invisible(midpoints)
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



