
setMethod("addComplement", signature(x = "transactions"),
    function(x, labels, complementLabels=NULL) {

	### default names for complements
	if(is.null(complementLabels)) complementLabels <- paste("!", 
		labels, sep="")
	
	add <- sapply(labels, function(y) !(x %in% y))
	m <- as.matrix(add)
	colnames(m) <- complementLabels
	tr <- as(m, "transactions")

	merge(x, tr)
    })

