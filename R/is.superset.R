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



## arules specific set methods: is.superset, is.subset (only for itemMatrix)
##
setMethod("is.superset", signature(x = "itemMatrix"),
    function(x, y = NULL, proper = FALSE) {
        if (is.null(y)) 
            t(is.subset(x, proper = proper))
        else 
            t(is.subset(y, x, proper))
    }
)

setMethod("is.superset", signature(x = "associations"),
    function (x, y = NULL, proper = FALSE)
        if (is.null(y))
            is.superset(items(x), proper = proper)
        else 
            is.superset(items(x), items(y), proper)
)

## this takes about 3 times the memory but is very fast!
## I suspect internally it always uses a lot of memory.
setMethod("is.subset", signature(x = "itemMatrix"),
    function(x, y = NULL, proper = FALSE) {
        if (length(x) == 0 || (!is.null(y) && length(y) == 0))
            return(logical(0))
        if (is.null(y)) 
            m <- .Call("R_crosstab_ngCMatrix", x@data, NULL, FALSE)
        else {
            ## conform
            k <- match(itemLabels(y), itemLabels(x))
            n <- which(is.na(k))
            if (length(n)) {
                k[n] <- x@data@Dim[1] + seq(length(n))
                x@data@Dim[1] <- x@data@Dim[1] + length(n)
            }
            if (any(k != seq_len(length(k))))
                y@data <- .Call("R_recode_ngCMatrix", y@data, k)
            if (y@data@Dim[1] <  x@data@Dim[1])
                y@data@Dim[1] <- x@data@Dim[1]

            m <- .Call("R_crosstab_ngCMatrix", x@data, y@data, FALSE)
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
    function(x, y = NULL, proper = FALSE) 
        if (is.null(y)) 
            is.subset(items(x), proper = proper)
        else 
            is.subset(items(x), items(y), proper)
)

###
