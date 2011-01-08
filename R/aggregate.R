## aggregate items to item groups
setMethod("aggregate", signature(x = "itemMatrix"),
	function(x, itemLabels) {
	    itemLabels <- as.factor(itemLabels)

	    if(length(itemLabels) != nitems(x)) stop("Supplied number of itemLabels does not match number of items in x!")

	    aggrMat <- as(sapply(levels(itemLabels),
			    FUN = function(l) as.numeric(itemLabels == l)), 
		    "dgCMatrix")

	    x@data <- as(crossprod(aggrMat, as(x, "dgCMatrix")), "ngCMatrix")
	    x@itemInfo <- data.frame(labels = levels(itemLabels))

	    validObject(x)
	    x


	})

setMethod("aggregate", signature(x = "itemsets"),
	function(x, itemLabels) {

	    items(x) <- aggregate(items(x), itemLabels)
	    x <- unique(x)
	    ## first support value is used

	    validObject(x)
	    x

	})

setMethod("aggregate", signature(x = "rules"),
	function(x, itemLabels) {

	    ##rhs(x) <- aggregate(rhs(x), itemLabels)
	    x@rhs <- aggregate(rhs(x), itemLabels)
	    lhs(x) <- aggregate(lhs(x), itemLabels)
	    x <- unique(x)
	    ## first support value is used

	    validObject(x)
	    x

	})



