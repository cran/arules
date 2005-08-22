
###*****************************************************
### return item frequency in a set
setMethod("itemFrequency", signature(x = "itemMatrix"),
    function(x, type = c("relative", "absolute")) {
      type <- match.arg(type)

      supports <- tabulate(x@data@i+1, nbins = x@data@Dim[1])
      names(supports) <- itemLabels(x)
       
      switch(type,
	     relative =  supports/dim(x)[1],
	     absolute =  supports)
      })

###*****************************************************
### return item frequency in tidLists
setMethod("itemFrequency", signature(x = "tidLists"),
    function(x, type= c("relative", "absolute")) {
    type <- match.arg(type)

    supports <-  size(x)
    names(supports) <- itemLabels(x)

    switch(type,
      relative =  supports/dim(x)[2],
      absolute =  supports)
    })


###*****************************************************
### plot item frequency
setMethod("itemFrequencyPlot", signature(x = "itemMatrix"),
    function(x, type = c("relative", "absolute"),  
      population = NULL, deviation = FALSE, horiz = FALSE,
      cex.names =  par("cex.axis"), xlab = NULL, ylab = NULL, ...) {
      
      type <- match.arg(type)
      
      # force relative for deviation
      if(deviation == TRUE) type <- "relative"
    
      # get frequencies
      itemFrequency <- itemFrequency(x, type)
      if(!is.null(population))
      	population.itemFrequency <- itemFrequency(population, type)

      # regular plot
      if(deviation == FALSE) {
          label <- paste("item frequency (", type, ")", sep="")
      }else{

          # show relative deviations instead of frequencies
	  if(is.null(population)) 
	  	stop("population needed for plotting deviations!")
	  itemFrequency <- (itemFrequency - population.itemFrequency) / 
	     population.itemFrequency
          label <- paste("relative deviation from population", sep="")
      }

    
      # make enough space for item labels
      maxLabel <- max(strwidth(names(itemFrequency), units = "inches", 
      	cex = cex.names))
      op.mai <- par("mai")
      if (horiz == FALSE) {
      	par(mai = c(maxLabel+0.5, op.mai[-1]))
        if(is.null(ylab)) ylab <- label	
      }else{
        par(mai = c(op.mai[1], maxLabel+0.5, op.mai[-c(1,2)]))
        if(is.null(xlab)) xlab <- label
      }
      
      midpoints <- barplot(itemFrequency, 
        las = 2, cex.name = cex.names, horiz = horiz,
	xlab = xlab, ylab = ylab, ...)
      
      # add population means
      if(!is.null(population) && deviation == FALSE)
        if(horiz == FALSE) lines(midpoints, population.itemFrequency)
        else lines(population.itemFrequency, midpoints)
      
      
      # reset image margins
      par(mai = op.mai)
      
      # return mitpoints
      invisible(midpoints)
      })


