### subset method
setMethod("[", signature(x = "dgCMatrix"), 
   function(x, i, j, ..., drop) {
   
   ### drop argument is not implemented jet
   if (missing(i)) i <- 1:x@Dim[1]
   if (missing(j)) j <- 1:x@Dim[2]
   
   z <- .Call("csc_subset", x, i, j, PACKAGE = "arules")

   ### set dimnames
   rownames(z) <- rownames(x)[i]
   colnames(z) <- colnames(x)[j]
   ### other elements missing (e.g., factor)?

   z
   })


