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
## Cross-tabulate joint purchases across pairs of items
##

setMethod("crossTable", signature(x = "itemMatrix"),
    function(x) {
        m <- .Call("R_crosstab_ngCMatrix", x@data, NULL, TRUE)
        if (is.null(dimnames(m)))
            dimnames(m) <- list(itemLabels(x), itemLabels(x))
        m
    }
)

###
