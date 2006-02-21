###*******************************************************************
### general dissimilarity method

#####*****************************************************************
### dissimilarity between transactions or items
setMethod("dissimilarity", signature(x = "matrix"),
    function(x, y = NULL, method = NULL, args = NULL) {
       ## Compute dissimilarities on binary data
       ## Returns a "S3 dist" object, for the time being.

       ## check input
       if (any(x != 0 && x !=1)) 
         stop("x is not a binary matrix!")

       ## cross dissimilarities?
       if (!is.null(y)) cross <- TRUE
       else cross <- FALSE
      
       
       builtin_methods <- c("Affinity", "Jaccard", "Matching", "Dice", 
	 "Euclidean")
       
       if(is.null(method)) ind <- 2      # Jaccard is standard
       else if(is.na(ind <- pmatch(tolower(method),
	  tolower(builtin_methods))))
    stop(gettextf("Value '%s' is not a valid abbreviation for a similarity method.",
	method), domain = NA)
   
      method <- builtin_methods[ind]
   
      ## affinity is special!
      if(ind == 1) {
       ## Affinity.
       ## Play some tricks so that methods for itemsets and rules can
       ## call us with given affinity matrix.
	affinities <- if(is.null(args)) affinity(x)
	  else args[[1]]
       ## Normalize transaction incidences by transaction length.
	  x <- x / pmax(rowSums(x), 1)
	  
	  if(cross == FALSE) {
	     dist <- 1 - as.dist(x %*% affinities %*% t(x))
	  }else{
	     y <- y/pmax(rowSums(y), 1)
	     dist <- new("ar_cross_dissimilarity", 
	     	1 - x %*% affinities %*% t(y))
	  }
	  
      ## Euclidean is special too
      } else if(ind == 5) {
        if(cross == TRUE) stop("Euclidean cross-distance not implemented.")
         
        dist <- dist(x, method = "euclidean")
	
      } else {
       
	if(cross != TRUE) y <- x
     
        ## prepare a, b, c, d (response table) for the rest of measures
 	## see: Gower, J. C. and P. Legendre.  1986.  Metric and 
	## Euclidean properties of dissimilarity coefficients.  
	## J. Classif. 3: 5 - 48. 
        a <- x %*% t(y)
        ### save some memory
	#b <- x %*% (1 - t(y))
	#c <- (1 - x) %*% t(y)
	#d <- ncol(x) - a - b - c

        # even faster code adapted from Leisch (2005): A toolbox for
	# K-centroids cluster analysis, Preprint.
        nx <- nrow(x) 
        ny <- nrow(y)
        a_b_c <- matrix(rowSums(x), nrow = nx, ncol = ny) +
		matrix(rowSums(y), nrow = nx, ncol = ny, byrow = TRUE) - a

      
      if(ind == 2) {
       	## Jaccard == binary (Sneath, 1957) 
       	#dist <- dist(as(x, "matrix"), "binary")
	
	#dist <- 1 - a/(a + b + c)
	dist <- 1 - (a/a_b_c)
	
      } else if(ind == 3){
        ## Matching Coefficient (Sokal and Michener, 1958)
	
	#we need d only here
	d <- ncol(x) - a_b_c
        
	#dist <- 1 - (a + d) / (a + b + c + d)  
	dist <- 1 - ((a + d) / (a_b_c + d))  
      
      } else if(ind == 4) {
        ## Dice Coefficient (Dice, 1945)
        #dist <- 1 - 2 * a / (2*a + b + c)
        dist <- 1 - (2 * a / (2*a_b_c))
	
      }
    }
   
      # in case we divided by zero
      dist[is.nan(dist)] <- 0
      
      if(cross == FALSE) {
      # return a S3 "dist" object just add "ar_dissimilarity"
        dist <- as.dist(dist)
	attr(dist, "method") <- method
	#class(dist) <- c("ar_dissimilarity", class(dist))
	return(dist)
      
      }else{ 
      # return a S4 "ar_cross_dist" object
        dist <- new("ar_cross_dissimilarity", dist, method = method)
        return(dist)
      }
      
    })


###*******************************************************************
### wrapper for itemMatrix (transactions)
setMethod("dissimilarity", signature(x = "itemMatrix"),
   function(x, y = NULL, method = NULL, args = NULL, 
     which = "transactions") {
     
     ## items instead of transactions
     if (pmatch(which, c("transactions", "items")) == 2) items <- TRUE
     else items <- FALSE
     
     x <- as(x, "matrix")
     if(items) x <- t(x) 
       
     if(!is.null(y)) { 
       y <- as(y, "matrix")
       if(items) y <- t(y) 
     }
     
     dissimilarity(x = x, y = y, method = method, args = args)
   })

###*******************************************************************
### wrapper for associations
setMethod("dissimilarity", signature(x = "associations"),
    function(x, y = NULL, method = NULL, args = NULL, 
      which = "transactions") {

      if(!is.null(y)) y <- items(y)

      dissimilarity(items(x), y, method = method, args = args, which = which)
    })   
    

###*******************************************************************
## Affinity
## For "Affinity" between items, see
## Aggarwal et al., "Finding Localized Association Rules in Market
## Basket Data".

setMethod("affinity", signature(x = "matrix"),
    function(x) {
      ## Affinity is basically the Jaccard similarity between items
      new("ar_similarity", 
	as.matrix(1 - dissimilarity(t(x), method = "Jaccard")),
      method = "Affinity")
      ## Fix: S4 as(..., "matrix") does not work
    })

setMethod("affinity", signature(x = "itemMatrix"),
    function(x) {
       affinity(as(x, "matrix"))
    })

