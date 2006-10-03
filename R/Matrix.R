## to list
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

        ## create a dgCMatrix
        i <- as.integer(unlist(from) - 1) # dgCMatrix starts with indes 0 
        p <- as.integer(c(0, cumsum(sapply(from, length))))

        new("dgCMatrix", i = i, p = p, 
            x = rep.int(1, times = length(i)),
            Dim = as.integer(c(max(i)+1, length(from))))

    })
