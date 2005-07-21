### distances and more for clustering

### general dissimilarity method
setMethod("dissimilarity", signature(x = "itemMatrix"),
    function(x, y = NULL, method = NULL, args = NULL) {
       ## Compute dissimilarities for transaction by item transaction
       ## incidence matrices x.  Currently, y is not used.

       ## For "Affinity"-based , see
       ## Aggarwal et al., "Finding Localized Association Rules in Market
       ## Basket Data".

       ## Return "just" a simple "dist" object, for the time being.
       ## Extend to methods functions a la CLUE eventually ...
    
    builtin_methods <- c("Jaccard", "Affinity")
    if(is.null(method))
    ## Jaccard is standard
    ind <- 1
    else if(is.na(ind <- pmatch(tolower(method),
	  tolower(builtin_methods))))
    stop(gettextf("Value '%s' is not a valid abbreviation for a similarity method.",
	method), domain = NA)
    
    if(ind == 2) {
       ## Affinity.
       ## Play some tricks so that methods for itemsets and rules can
       ## call us with given affinity matrix.
	affinities <- if(is.null(args)) affinity(x)
	  else args[[1]]
       ## Normalize transaction incidences by transaction length.
	  x <- as(x, "matrix")
	  x <- x / pmax(rowSums(x), 1)
	  as.dist(1 - x %*% affinities %*% t(x))
      }
      else
       ## binary == Jaccard 
	dist(as(x, "matrix"), "binary")
    })

### now for associations
setMethod("dissimilarity", signature(x = "associations"),
    function(x, y = NULL, method = NULL, args = NULL) {
    ## Fix: Once y is used, we need to do y = items(y), or so.
    dissimilarity(items(x), y = y, method = method, args = args)
    })
    

### define affinity
setMethod("affinity", signature(x = "itemMatrix"),
    function(x) {
    ## For "Affinity" between items, see
    ## Aggarwal et al., "Finding Localized Association Rules in Market
    ## Basket Data".
    
    ## Note, the dgCMatrix is already transposed
    ## Affinity is basically the Jaccard similarity between items
    as.matrix(1 - dist(t(as(x, "matrix")), "binary"))
    ## Fix: S4 as(..., "matrix") does not work
    })


### heatmap using Jaccard
setMethod("hmap", signature(x = "itemMatrix"),
    function(x, distfun = function(z) dist(z, method = "binary"),
    margins = c(10, 10), 
    xlab = "Items (Columns)", ylab = "Elements (Rows)",
    col = gray(seq(from = 1, to = 0, length = 100)), ...) {
    
    ### kill empty items
    x <- x[,itemFrequency(x) != 0]
    heatmap(as(x, "matrix"), distfun = distfun,
    	xlab = xlab, ylab = ylab,
	margins = margins, col = col, ...)
    })
