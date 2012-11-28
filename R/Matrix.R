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



## provide some interfaces to our C implementations


## t in Matrix is now faster
##setMethod("t", signature(x = "ngCMatrix"),
##    function(x) .Call("R_transpose_ngCMatrix", x, PACKAGE="arules"))

## overloading of [ for ngCMatrix cannot be accomplished
## easily as there are too many signatures to overload.


## density 
.density_Matrix <- function(x) length(x@i)/x@Dim[1]/x@Dim[2]

## helper to convert names into an integer index
.translate_index <- function(i, labels, n) {
    
    if(is.numeric(i) && all(i>0)) return(as.integer(i))
    
    if(is.logical(i)) return(which(i))
    
    if(!is.null(labels)) {
	sel <- seq(length(labels))
	names(sel) <- labels
    }else sel <- seq(n)    
    
    sel <- sel[i]

    if(any(is.na(sel))) stop("subscript out of bounds")
    sel
}



###
