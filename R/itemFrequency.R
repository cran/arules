#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyrigth (C) 2011 Michael Hahsler, Christian Buchta, 
#			Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.



##*******************************************************
## Functions itemFrequency and itemFrequencyPlot
##
## to calculate item frequencies (item support) and plot item frequencies

##*****************************************************
## return item frequency in a set
setMethod("itemFrequency", signature(x = "itemMatrix"),
    function(x, type = c("relative", "absolute")) {
        type <- match.arg(type)

        ## we could also use rowSums
        ##supports <- tabulate(x@data@i + 1L, nbins = x@data@Dim[1])
        
        supports <- .Call("R_rowSums_ngCMatrix", x@data)
        
        names(supports) <- itemLabels(x)

        switch(type,
            relative =  supports/length(x),
            absolute =  supports)
    })

##*****************************************************
## return item frequency in tidLists
setMethod("itemFrequency", signature(x = "tidLists"),
    function(x, type= c("relative", "absolute")) {
        type <- match.arg(type)

        supports <-  size(x)
        names(supports) <- 1:length(supports)

        switch(type,
            relative =  supports/dim(x)[2],
            absolute =  supports)
    })


##*****************************************************
## plot item frequency
setMethod("itemFrequencyPlot", signature(x = "itemMatrix"),
    function(x, type = c("relative", "absolute"), support = NULL, 
        topN = NULL, population = NULL, popCol = "black", popLwd = 1, 
        lift = FALSE, horiz = FALSE,
        names = TRUE, cex.names =  par("cex.axis"), 
        xlab = NULL, ylab = NULL, mai = NULL, ...) {

        type <- match.arg(type)

        ## force relative for lift
        if(lift == TRUE) type <- "relative"

        ## plot only items with support
        if(!is.null(support)) {
            if(!is.null(population)) {
                frequentItems <- itemFrequency(population, type) >= support
                population <- population[, frequentItems]
            } else frequentItems <- itemFrequency(x, type) >= support
            x <- x[, frequentItems]
        }

        ## get frequencies
        itemFrequency <- itemFrequency(x, type)
        if(!is.null(population))
        population.itemFrequency <- itemFrequency(population, type)

        ## regular plot
        if(lift == FALSE) {
            label <- paste("item frequency (", type, ")", sep="")
            offset <- 0

        }else{

            ## show lift instead of frequencies
            if(is.null(population)) 
            stop("population needed for plotting lift!")

            ## -1 and offset are used to draw bars smaller than one
            ## upside down
            itemFrequency <- (itemFrequency / population.itemFrequency) -1
            offset <- 1


            ## take care of div by zero
            itemFrequency[is.infinite(itemFrequency)] <- NaN   

            label <- "lift ratio"
        }

        ## plot only top n items (either itemFrequency or lift)
        if(!is.null(topN)) {
            take <- order(itemFrequency, decreasing = TRUE)[1:topN]
            itemFrequency <- itemFrequency[take] 
            if(!is.null(population))
            population.itemFrequency <- population.itemFrequency[take]
        }


        ## supress names
        if(names == FALSE) names(itemFrequency) <- NA

        if(horiz == FALSE) midpoints <- .barplot_vert(itemFrequency, ..., 
            offset = offset,
            cex.names = cex.names, xlab = xlab, 
            ylab = if(is.null(ylab)) label else ylab, 
            mai = mai) 

        else  midpoints <- .barplot_horiz(itemFrequency, ...,
            offset = offset,  
            cex.names = cex.names, xlab = if(is.null(xlab)) label else xlab, 
            ylab = ylab, mai = mai)



        ## add population means (we switch off clipping first!)
        if(!is.null(population) && lift == FALSE)
        if(horiz == FALSE) lines(midpoints, population.itemFrequency, 
            lwd = popLwd, col = popCol, xpd = TRUE)
        else lines(population.itemFrequency, midpoints, 
            lwd = popLwd, col = popCol, xpd = TRUE)

        ## return mitpoints
        invisible(midpoints)
    })


## helper functions for barplot
.barplot_vert <- function(height, ..., 
    cex.names = par("cex.axis"), xlab = NULL, ylab = NULL, mai = NULL){

    labels <- names(height)

    ## for neg. heights we use straight labels
    if(min(height, na.rm = TRUE) < 0) straight <- TRUE
    else straight <- FALSE

    op.mai <- par("mai")
    if(is.null(mai)) {
        mai <- op.mai
        if (straight == TRUE) mai[1] <- max(strwidth(labels, units = "inches",
                cex = cex.names)) + min(par("fin")[2]*0.1, 0.5)
        else mai[1] <- max(strwidth(labels, units = "inches",
                cex = cex.names)) / 2^.5 + min(par("fin")[2]*0.1, 0.5)
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

    ## make enough space for item labels
    op.mai <- par("mai")
    if(is.null(mai)) {
        mai <- op.mai
        mai[2] <- max(strwidth(names(height), units = "inches", 
                cex = cex.names)) + min(par("fin")[1]*0.1, 0.5)

    }
    par(mai = mai) 
    on.exit(par(mai = op.mai))

    midpoints <- barplot(height, 
        las = 2, cex.name = cex.names, horiz = TRUE,
        xlab = xlab, ylab = ylab, ...)

    invisible(midpoints)
}


