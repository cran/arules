##*******************************************************
## predict
##
## object ... medoids (no labels needed) or examples (labels needed)
## newdata ... objects to predict labels for
## ... ... for dissimilarity, e.g., method

setMethod("predict", signature(object = "itemMatrix"),
    function(object, newdata, labels = NULL, blocksize = 200, ...) {

        lenOb <- length(object) 
        lenNew <- length(newdata)

        ## memory requirements for dissimilarity (see proximities.R)
        ## total w/o input: about 5 * nx * ny * 8 byte
        ## required memory in MB
        ## reqMemMB <- 5 * lenOb * lenNew * 8 / 1024 / 1024
        blocksize <- floor(blocksize * 1024 * 1024 / 5 / lenOb / 8)

        if(blocksize < 1) 
        stop("Too many examples in object. Increase usable memory blocksize!") 

        if(is.null(labels)) labels <- 1 : lenOb

        # do it in one run
        if(lenOb*lenNew <= blocksize) {
            xd <- dissimilarity(newdata, object, ...)
            return(labels[max.col(-xd)])
        }

        # do it in blocks
        newLabels <- integer(lenNew)

        blockStart <- 1
        while(blockStart < lenNew) {
            blockEnd <- min(blockStart+blocksize, lenNew)
            xd <- dissimilarity(newdata[blockStart:blockEnd], object, ...)
            newLabels[blockStart:blockEnd] <- labels[max.col(-xd)] 
            blockStart <- blockEnd
        }

        return(newLabels)	


        ## check labels stuff
    })



