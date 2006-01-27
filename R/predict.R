###*******************************************************
### predict
###
### object ... medoids (no labels needed) or examples (labels needed)
### newdata ... objects to predict labels for
### ... ... for dissimilarity, e.g., method

setMethod("predict", signature(object = "itemMatrix"),
    function(object, newdata, labels = NULL, ...) {

        ### maximal size of the proximity matrix to 
        MAXBLOCK <- 5000000  
	lenOb <- length(object) 
        lenNew <- length(newdata)
	
        if(lenOb >= MAXBLOCK) stop("Too many examples (>=5000000).") 
      
        if(is.null(labels)) labels <- 1 : lenOb

	# do it in one run
	if(lenOb*lenNew < MAXBLOCK) {
	  xd <- dissimilarity(newdata, object, ...)
	  return(labels[max.col(-xd)])
	}

	# do it in blocks
	newLabels <- integer(lenNew)

	blockLength <- floor(MAXBLOCK / lenOb)
	print(blockLength)
	blockStart <- 1
	while(blockStart < lenNew) {
	  blockEnd <- min(blockStart+blockLength, lenNew)
	  xd <- dissimilarity(newdata[blockStart:blockEnd], object, ...)
	  newLabels[blockStart:blockEnd] <- labels[max.col(-xd)] 
	  blockStart <- blockEnd
	}
        
	return(newLabels)	
	
   
### check labels stuff
     })



