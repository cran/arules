###*****************************************************************
### Functions union, intersect, setequal 
###
### set operations

### makes no sense for itemMatrix
#setMethod("union", signature(x = "itemMatrix", y = "itemMatrix"),
    #    function(x, y) {
	#unique(c(x, y))
	#})

setMethod("union", signature(x = "associations", y = "associations"),
    function(x, y) {
    unique(c(x, y))
    })

#setMethod("intersect", signature(x = "itemMatrix", y = "itemMatrix"),
    #    function(x, y) {
	#u <- c(x, y)
	#u[duplicated(u)]
	#})

setMethod("intersect", signature(x = "associations", y = "associations"),
    function(x, y) {
    u <- c(x, y)
    u[duplicated(u)]
    })

#setMethod("setequal", signature(x = "itemMatrix", y = "itemMatrix"),
    #    function(x, y) {
	# 
	#   if(length(x) != length(y)) return(FALSE) 

	#   if(length(union(x,y)) == length(x)) return(TRUE)
	#   return(FALSE)    
	#})

setMethod("setequal", signature(x = "associations", y = "associations"),
    function(x, y) {
    if(length(x) != length(y)) return(FALSE) 

    if(length(union(x,y)) == length(x)) return(TRUE)
    return(FALSE)    
    })

