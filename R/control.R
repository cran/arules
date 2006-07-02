###*******************************************************
### Classes AScontrol, APcontrol, ECcontrol
###
### control parameters for the apriori and eclat functions
### + superclass (AScontol)


###**********************************************************
### coercion
setAs("NULL", "APcontrol",
function(from, to) { new(to) })

setAs("list", "APcontrol", function(from, to) .list2object(from, to))

setAs("NULL", "ECcontrol",
function(from, to) { new(to) })

setAs("list", "ECcontrol", function(from, to) .list2object(from, to))

###**********************************************************
### initialize
setMethod("initialize", "AScontrol",
   function(.Object, sort, ...) {
     if (!missing(sort)) {
     if (sort - as.integer(sort)) stop("sort = ", sort, 
       " can not be coerced to integer without error.")
       sort = as.integer(sort)
       .Object <- callNextMethod(.Object, sort = sort, ...)
     }
     else .Object <- callNextMethod(.Object, ...)
     .Object
   })


###**********************************************************
### show

setMethod("show", signature(object = "AScontrol"),
    function(object) {
    print(data.frame(sapply(slotNames(object), 
     function(x) slot(object, x), simplify = FALSE), row.names = ""))
    invisible(object)
    })




