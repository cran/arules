## image plot

## NOTE Matrix coerces to dgTMatrix and then uses levelplot
setMethod("image", signature(x = "itemMatrix"),
    function(x, xlab = "Items (Columns)", ylab = "Elements (Rows)", ...) 
    image(t(x@data), sub = NULL, ylab = ylab, xlab = xlab, ...)
)

setMethod("image", signature(x = "transactions"),
    function(x, ...)
    image(as(x, "itemMatrix"), xlab = "Items (Columns)", 
        ylab = "Transactions (Rows)", ...)
)

setMethod("image", signature(x = "tidLists"),
    function(x, xlab="Transactions (Columns)",
        ylab="Items/itemsets (Rows)", ...)
        image(t(x@data), sub = NULL, colorkey = FALSE, 
            ylab = ylab, xlab = xlab, ...)
)


