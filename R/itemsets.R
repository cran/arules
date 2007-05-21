##*******************************************************
## Class itemsets
##
## a set of itemsets, subclass of associations



##************************************************
## dimensions
setMethod("length", signature(x = "itemsets"),
    function(x) length(x@items))

setMethod("size", signature(x = "itemsets"),
    function(x) size(x@items))

##***********************************************
## coercion
setAs("itemsets", "data.frame", 
    function(from) {
        if(!length(from)) return (data.frame())

        if(!length(from@quality)) return(data.frame(items = labels(from)))
        return(data.frame(items = labels(from), from@quality))
    })


##***********************************************
## labels

setMethod("labels", signature(object = "itemsets"),
    function(object, ...) labels(object@items, ...)$elements)

setMethod("itemLabels", signature(object = "itemsets"),
    function(object) itemLabels(items(object)))


##************************************************
## accessors


setMethod("itemInfo", signature(object = "itemsets"),
    function(object) return(itemInfo(object@items)))

setMethod("items", signature(x = "itemsets"),
    function(x) x@items)

setReplaceMethod("items", signature(x = "itemsets"),
    function(x, value) {
        x@items <- value
        x
    })


setMethod("tidLists", signature(x = "itemsets"),
    function(x) x@tidLists)


##****************************************************
## subset, combine, duplicated, match

setMethod("[", signature(x = "itemsets", i = "ANY", j = "ANY", drop = "ANY"),
        function(x, i, j, ..., drop)            # ]
    {
        if (!missing(j)) stop("incorrect number of dimensions (j not possible)")
        if (missing(i)) return(x)
        y <- x
        slots <- intersect(slotNames(x), c("items", "tidLists"))
        for (sl in slots) slot(y, sl) <- slot(x, sl)[i]
        y@quality <- x@quality[i,,drop=FALSE]
        return(y)
    })

## auxilliary function to combine 2 quality data.frames
#.combineQuality <- function(a, b) {
#    namesA <- names(a)
#    namesB <- names(b)
#
#    ## do we have a NULL data.frame
#    if(length(a) == 0) a <- b 
#    if(length(b) == 0) b <- a 
#    ## Note: the copied values will be set to NA in the following    
#
#    if(!setequal(namesA, namesB)) {
#        ## in a but not in b
#        diffAB <- setdiff(namesA, namesB)
#        if(length(diffAB) > 0 ) 
#        sapply(diffAB, FUN = function(x) b[[x]] <<- NA)
#
#        ## in b but not in a  
#        diffBA <- setdiff(namesB, namesA) 
#        if(length(diffBA) > 0 ) 
#        sapply(diffBA, FUN = function(x) a[[x]] <<- NA)
#    }
#
#    rbind(a, b)
#}

## FIXME this is inefficient for data.frames with many
##       rows. we do not handle cases with non-zero 
##       rows/columns and zero columns/rows.

.combineMeta <- function(x, y, name, ...) {
    if (length(slot(x, name))) {
        if (length(slot(y, name)))
            slot(x, name) <- rbind(slot(x, name), slot(y, name))
        else {
            k <- rbind(NA, slot(x, name))
            slot(x, name) <- 
                rbind(slot(x, name), k[rep(1, length(y)),, drop = FALSE])
        }
    } else
    if (length(slot(y, name))) {
        k <- rbind(NA, slot(y, name))
        slot(x, name) <- 
            rbind(k[rep(1, length(x)),, drop = FALSE],  slot(y, name))
    }
    slot(x, name)
}


setMethod("c", signature(x = "itemsets"),
    function(x, ..., recursive = FALSE){
#
#        args <- list(...)  
#        if(length(args) == 0) return(x)  
#
#        # build quality data.frame first
#        q <- x@quality
#        for(i in 1:length(args)) {
#            q <- .combineQuality(q, args[[i]]@quality)
#        }
#
#        # create joint itemMatrix
#        z <- lapply(list(...), FUN = function(i) i@items)
#        new("itemsets", items = c(x@items, z, recursive = TRUE), 
#            quality = q)

        args <- list(...)
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!inherits(y, "itemsets"))
                stop("can combine itemsets only")
            x <- new("itemsets", items   = c(x@items, y@items), 
                                 quality = .combinMeta(x, y, "quality"))
        }
        validObject(x)
        x
    })


setMethod("duplicated", signature(x = "itemsets"),
    function(x, incomparables = FALSE, ...) {
        duplicated(x@items, 
            incomparables = incomparables, ...)
    })

setMethod("match", signature(x = "itemsets", table = "itemsets"),
    function(x,  table, nomatch = NA, incomparables = FALSE) {
        match(x@items, table@items,
            nomatch = nomatch, incomparables = incomparables)
    })




##************************************************
## show / summary



setMethod("summary", signature(object = "itemsets"), 
    function(object, ...) {
        new("summary.itemsets", 
            length = length(object),
            items = summary(object@items,  ...),
            quality = if(length(object@quality) > 0) summary(object@quality)
            else summary(NULL),
            tidLists = !is.null(object@tidLists))
    })

setMethod("show", signature(object = "summary.itemsets"), 
    function(object) {
        cat("set of", object@length,"itemsets\n")

        if(object@length>0) {     
            cat("\nmost frequent items:\n")
            print(object@items@itemSummary)
            cat("\nelement (itemset/transaction) length distribution:")
            print(object@items@lengths)

            cat("\n")
            print(object@items@lengthSummary)

            cat("\nsummary of quality measures:\n")
            print(object@quality)
            cat("\nincludes transaction ID lists:",object@tidLists,"\n")
        }
    })


