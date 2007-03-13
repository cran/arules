## to list
setAs("ngCMatrix", "list",
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

setAs("list", "ngCMatrix",
    function(from) {

        ## create a ngCMatrix
        i <- as.integer(unlist(from) - 1) # ngCMatrix starts with index 0 
        p <- as.integer(c(0, cumsum(sapply(from, length))))

        new("ngCMatrix", i = i, p = p, 
            Dim = as.integer(c(max(i)+1, length(from))))

    })
