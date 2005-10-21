###*******************************************************
### Functions itemFrequency and itemFrequencyPlot
###
### to calculate item frequencies (item support) and plot item frequencies

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
      cex.names =  par("cex.axis"), xlab = NULL, ylab = NULL, mai = NULL, ...) {
      
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

      if(horiz == FALSE) midpoints <- .barplot_vert(itemFrequency, ...,
	  cex.names = cex.names, xlab = xlab, ylab = label, mai = mai) 
      
      else  midpoints <- .barplot_horiz(itemFrequency, ...,
          cex.names = cex.names, xlab = label, ylab = ylab, mai = mai)
   
      
      # add population means
      if(!is.null(population) && deviation == FALSE)
        if(horiz == FALSE) lines(midpoints, population.itemFrequency)
        else lines(population.itemFrequency, midpoints)
      
      
      
      # return mitpoints
      invisible(midpoints)
      })


### helper functions for barplot
.barplot_vert <- function(height, ..., 
  cex.names = par("cex.axis"), xlab = NULL, ylab = NULL, mai = NULL){

  labels <- names(height)

  ## for neg. heights we use straight labels
  if(min(height) < 0) straight <- TRUE
  else straight <- FALSE
  
  op.mai <- par("mai")
  if(is.null(mai)) {
    mai <- op.mai
    if (straight == TRUE) mai[1] <- max(strwidth(labels, units = "inches",
	    cex = cex.names)) + 0.5
    else mai[1] <- max(strwidth(labels, units = "inches",
	    cex = cex.names)) / 2^.5 + 0.5
  } 
  par(mai = mai) 
  on.exit(par(mai = op.mai))

  ## Create plot with no x axis and no x axis label
  
  if(straight == TRUE)
      bp <- barplot(height, ...,  las=2, cex.names = cex.names, 
	  xlab = xlab, ylab = ylab)
  
  else {
      bp <- barplot(height, ..., xaxt = "n",  xlab = "", ylab = ylab)

      ## move down from the lower end of the plot by 1/20 of the plotting
      ## area for labels
      text(bp, par("usr")[3] - (par("usr")[4] - par("usr")[3]) / 20, 
	  srt = 45, adj = 1,
	  labels = labels, xpd = TRUE, cex = cex.names)

      ## Plot x axis label
      mtext(1, text = xlab, line = par("mar")[1]-1)
  }
  invisible(bp)
}

.barplot_horiz <- function(height, ..., 
  cex.names = par("cex.axis"), xlab = NULL, ylab = NULL, mai = NULL){

  # make enough space for item labels
  op.mai <- par("mai")
  if(is.null(mai)) {
    mai <- op.mai
    mai[2] <- max(strwidth(names(height), units = "inches", 
	cex = cex.names)) + 0.5

  }
  par(mai = mai) 
  on.exit(par(mai = op.mai))

  midpoints <- barplot(height, 
    las = 2, cex.name = cex.names, horiz = TRUE,
    xlab = xlab, ylab = ylab, ...)

  invisible(midpoints)
}


