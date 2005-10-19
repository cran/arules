###*******************************************************
### Function [ for dgCMatrix
###
### additional subset functionality for dgCMatrix


### subset method directly on a dgCMatrix
setMethod("[", signature(x = "dgCMatrix", i = "ANY", j = "ANY", drop = "ANY"), 
   function(x, i, j, ..., drop) {
   
   ### drop argument is not implemented, we dont need it
   if(missing(j) && missing(i)) return(x)

   ### this should take care of everything "[" can do:
   ### neg. indices, 0, etc.
   if (missing(i)) i <- 1 : x@Dim[1]
   else i <- c(1 : x@Dim[1])[i]

   if (missing(j)) j <- 1 : x@Dim[2]
   else j <- c(1 : x@Dim[2])[j]

   z <- .Call("dgC_subset", x, i, j, PACKAGE = "arules")

   ### set dimnames
   rownames(z) <- rownames(x)[i]
   colnames(z) <- colnames(x)[j]
   ### other elements missing (e.g., factor)?

   z
   })

### to list
setAs("dgCMatrix", "list",
  function(from) {
    data <- from
    
    z <- vector(length = (data@Dim[2]), mode = "list")
    l <- which(diff(data@p) != 0)
    sapply(l, function(i)
	  z[[i]] <<- as.integer(data@i[(data@p[i]+1):data@p[i+1]]+1),
	  simplify=FALSE)
    # +1 since i starts with index 0 instead of 1
    
    return(z)
  })

setAs("list", "dgCMatrix",
    function(from) {

        ### create a dgCMatrix
        i <- as.integer(unlist(from) - 1) # dgCMatrix starts with indes 0 
        p <- as.integer(c(0, cumsum(sapply(from, length))))

        new("dgCMatrix", i = i, p = p, 
            x = rep.int(1, times = length(i)),
            Dim = as.integer(c(max(i)+1, length(from))))

    })
