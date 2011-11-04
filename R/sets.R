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



##*****************************************************************
## Basic set operations:  union, intersect, setequal, ... 
## as defined in base; worke now for all classes which implement 
## unique, match and length (in arules associations and itemMatrix). 
##


setMethod("union", signature(x = "ANY", y = "ANY"),
    function(x, y) unique(c(x, y)) 
) 

setMethod("intersect", signature(x = "ANY", y = "ANY"),
    function(x, y) unique(y[match(x, y, 0)])
)

setMethod("setequal", signature(x = "ANY", y = "ANY"),
    function(x, y) all(c(match(x, y, 0) > 0, match(y, x, 0) > 0))
)

setMethod("setdiff", signature(x = "ANY", y = "ANY"),
    function(x, y) 
    unique(if (length(x) || length(y)) x[match(x, y, 0) == 0] else x)
)

setMethod("is.element", signature(el = "ANY", set = "ANY"),
    function(el, set) match(el, set, 0) > 0 
)


## 

