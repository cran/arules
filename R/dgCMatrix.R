### subset method
setMethod("[", signature(x = "dgCMatrix"), 
   function(x, i, j, ..., drop) {
   mdrop <- missing(drop)
   if (!mdrop)
	warning("drop argument will be ignored")
   if (missing(i)) i <- 1:x@Dim[1]
   if (missing(j)) j <- 1:x@Dim[2]
   z <- .Call("csc_subset", x, i, j, PACKAGE = "arules")
   z
   })



