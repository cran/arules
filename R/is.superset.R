#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011, 2012 Michael Hahsler, Christian Buchta, 
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



## arules specific set methods: is.superset, is.subset (only for itemMatrix)
##
setMethod("is.superset", signature(x = "itemMatrix"),
    function(x, y = NULL, proper = FALSE, sparse = FALSE) 
	if (is.null(y)) t(is.subset(x, NULL, proper, sparse))
	else t(is.subset(y, x, proper, sparse))
	    )

setMethod("is.superset", signature(x = "associations"),
    function (x, y = NULL, proper = FALSE, sparse = FALSE)
	if (is.null(y)) t(is.subset(x, NULL, proper, sparse))
	else t(is.subset(y, x, proper, sparse))
)

## this takes about 3 times the memory but is very fast!
## I suspect internally it always uses a lot of memory.
setMethod("is.subset", signature(x = "itemMatrix"),
    function(x, y = NULL, proper = FALSE, sparse = FALSE) {
        if (length(x) == 0 || (!is.null(y) && length(y) == 0))
            return(logical(0))
        
	## y needs to be itemMatrix
	if(!is.null(y) && !is(y, "itemMatrix")) y <- items(y)
	
	if(sparse) return(.is.subset_sparse(x, y, proper))
	
	if (is.null(y)) 
            m <- .Call("R_crosstab_ngCMatrix", x@data, NULL, FALSE, 
		PACKAGE="arules")
        else {
            ## conform
            k <- match(itemLabels(y), itemLabels(x))
            n <- which(is.na(k))
            if (length(n)) {
                k[n] <- x@data@Dim[1] + seq(length(n))
                x@data@Dim[1] <- x@data@Dim[1] + length(n)
            }
            if (any(k != seq_len(length(k))))
                y@data <- .Call("R_recode_ngCMatrix", y@data, k,
		    PACKAGE="aruels")
            if (y@data@Dim[1] <  x@data@Dim[1])
                y@data@Dim[1] <- x@data@Dim[1]

            m <- .Call("R_crosstab_ngCMatrix", x@data, y@data, FALSE,
		    PACKAGE="arules")
        }
        m <- m == size(x)

        if (proper == TRUE) 
            if (is.null(y)) 
                m <- m & outer(size(x), size(x), "<")
            else 
                m <- m & outer(size(x), size(y), "<")
        ## if we were sure that x and y contain no duplicates ...
        ## we never can be sure [ceeboo 2007]
        m
    }
)

setMethod("is.subset", signature(x = "associations"),
    function(x, y = NULL, proper = FALSE, sparse = FALSE) 
            is.subset(items(x), y, proper, sparse)
)

### use tidlist intersection 
.is.subset_sparse <- function(x, y = NULL, proper=FALSE) { 

    if(is.null(y)) y <- x

    xitems <- LIST(x, decode = FALSE)
    ylists <- LIST(as(y, "tidLists"), decode = FALSE)

    ## select tid-lists for items and do intersection
    contained <- lapply(xitems, FUN = function(i) {
		if(length(i)==0) ssid <- 1:nrow(y)
		else { 
		    tidls <- unlist(ylists[i])
		    if(is.null(tidls)) ssid <- integer(0)
		    else ssid <- which(tabulate(tidls) == length(i))
		    }

		if(proper) ssid <- ssid[size(y)[ssid] > length(i)]
		ssid
	    })

    .list2ngCMatrix(contained, nrow(y))
}


### convert a list into a ngCMatrix
.list2ngCMatrix <- function(from, max=NULL) {
    from <- lapply(from, sort)
    p <- cumsum(sapply(from, length))

    i <- as.integer(unlist(from, use.names = FALSE))

    if(is.null(max)) max <- max(i)

    t(new("ngCMatrix", p   = c(0L, p),
		    i   = i - 1L,
		    Dim = c(as.integer(max), length(from))))
}



###
