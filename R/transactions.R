###*****************************************************
### coercions

setAs("matrix", "transactions",
    function(from) {
    new("transactions", as(from, "itemMatrix"), 
     transactionInfo = data.frame(transactionIDs = dimnames(from)[[1]]))
    })

setAs("transactions", "matrix",
    function(from) {
    m <- as(as(from, "itemMatrix"), "matrix")
    if (!is.null(from@transactionInfo[["transactionIDs"]])) 
    	dimnames(m)[[1]] <- from@transactionInfo[["transactionIDs"]]
    return(m)
    })
		      

setAs("list", "transactions",
	  function(from) {
	  new("transactions", as(from, "itemMatrix"), 
	       transactionInfo = data.frame(transactionIDs = names(from)))
	  })

setAs("transactions", "list",
    function(from) {
    LIST(from, decode = TRUE)
    })

setMethod("LIST", signature(from = "transactions"),
	  function(from, decode = TRUE) {
	  l <- LIST(as(from, "itemMatrix"), decode)
	  if(decode == TRUE) 
	    names(l) <- from@transactionInfo[["transactionIDs"]]
	  return(l)
	  })

setAs("data.frame", "transactions", function(from) {
    if (!all(sapply(from, is, "factor")))
      stop("Column ", names(which(!sapply(from, is, "factor"))),
      " is not a factor.")
    
    from_levels <- sapply(from, levels, simplify = FALSE)
    assign <- sapply(from_levels, length, USE.NAMES = FALSE)
    to_levels <- unlist(from_levels, use.names = FALSE)
    to_vars <- rep(names(from), assign)
    to_labels <- paste(to_vars, to_levels, sep = " = ")
    
    lev <- c(0, cumsum(assign))
    to_dim <- c(length(to_labels),dim(from)[1])
    len <- rep(dim(from)[2], to_dim[2])
    v <- lapply(1:dim(from)[2], function(i) factor(from[[i]],
	levels = levels(from[[i]]),
	labels = lev[i]:(lev[i+1]-1)))
    v <- data.frame(v)
    i <- as.integer(t(v))

    if (any(is.na(v))) {
      i <- i[!is.na(i)]
      ind <- table(which(is.na(v), arr.ind = TRUE)[,1])
      rowsNA <- as.integer(names(ind))
      len[rowsNA] <- len[rowsNA] - ind
    }

    p <- as.integer(c(0, cumsum(len)))
    z <- new("dgCMatrix", x = rep(as.numeric(1), length(i)),
      i = i, p = p, Dim = to_dim)
    cat("Recoded",dim(from)[2],"variables to",to_dim[1],"binary items\n")
    new("transactions", new("itemMatrix", data = z,
      itemInfo=data.frame(labels = to_labels, variables = to_vars, 
      levels=to_levels)))
})


###*****************************************************
### subset

setMethod("[", signature(x = "transactions"),
    function(x, i, j, ..., drop) {
    if(missing(i)) i <- c(1:length(x))
    new("transactions",as(x, "itemMatrix")[i,j,...,drop=drop],
    	transactionInfo = x@transactionInfo[i,,drop=FALSE])
    })


###*****************************************************
### show / summary


setMethod("show", signature(object = "transactions"),
	function(object) {
	cat("transactions in sparse format with\n",
		dim(object)[1],"transactions (rows) and\n",
		dim(object)[2],"items (columns)\n")
	})

setMethod("image", signature(x = "transactions"),
   function(x, ...) {
   image(as(x,"itemMatrix"), xlab="Items (Columns)", 
     ylab="Transactions (Rows)")
})

setMethod("summary", signature(object = "transactions"),
   function(object, ...) {
   new("summary.transactions", summary(as(object,"itemMatrix")),
   	transactionInfo = transactionInfo(object[1:min(3, length(object))]))
})

setMethod("show", signature(object = "summary.transactions"),
   function(object) {
      cat("transactions as ")
      show(as(object,"summary.itemMatrix"))

      if(length(names(object@transactionInfo)) > 0) {
      cat("\nincludes extended transaction information - examples:\n")
      print(object@transactionInfo)}
    })



###*****************************************************
### accessors

			    
setMethod("transactionInfo", signature(x = "transactions"),
    function(x) {
    x@transactionInfo
    })

setReplaceMethod("transactionInfo", signature(x = "transactions"),
    function(x, value) {
    x@transactionInfo <- value
    x
    })


###***************************************************************
### read function

read.transactions <-
function(file, format = c("basket", "single"), sep = NULL, cols = NULL)
{
    format <- match.arg(format)
    if(format == "basket") {
        if(is.null(sep)) sep <- "[ \t]+"
        return(as(strsplit(readLines(file), split = sep),
                  "transactions"))
    }
    ## If format is "single", have lines with TIDs and IIDs in the
    ## columns specified by 'cols'.
    if(!(is(cols, "numeric") && (length(cols) == 2)))
        stop("'cols' must be a numeric vector of length 2.")
    cols <- as(cols, "integer")
    ## Thanks to BDR for indicating how to only read in the relevant
    ## columns.
    what <- vector("list", length = max(cols))
    what[cols] <- ""
    entries <- scan(file = file, sep = sep, what = what, flush = TRUE,
                    quiet = TRUE)
    as(split(entries[[cols[2]]], entries[[cols[1]]]), "transactions")
}
