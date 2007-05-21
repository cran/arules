##*******************************************************
## Class rules
##
## a set of rules, subclass of associations


##************************************************
## dimensions
setMethod("length", signature(x = "rules"),
    function(x) length(x@lhs)
    )

setMethod("size", signature(x = "rules"),
    function(x, ...) size(x@lhs) + size(x@rhs)
    )


##***********************************************
## coercion
setAs("rules", "data.frame",
    function(from) {
        if(!length(from)) return (data.frame())

        if(!length(from@quality)) return(data.frame(rules = labels(from)))
        data.frame(rules = labels(from), from@quality)
    })

##***********************************************
## labels

setMethod("labels", signature(object = "rules"),
    function(object, ruleSep = " => ",...) {
        paste(labels(object@lhs, ...)$elements, ruleSep,
            labels(object@rhs, ...)$elements, sep="")
    })

setMethod("itemLabels", signature(object = "rules"),
    function(object) {
        itemLabels(lhs(object))
    })


##************************************************
## accessors

setMethod("itemInfo", signature(object = "rules"),
    function(object) {
        return(itemInfo(object@lhs))
    })


setMethod("lhs", signature(x = "rules"),
    function(x) {
        x@lhs
    })

setReplaceMethod("lhs", signature(x = "rules"),
    function(x, value) {
        x@lhs <- value
        x
    })

setMethod("rhs", signature(x = "rules"),
    function(x) {
        x@rhs
    })

setReplaceMethod("rhs", signature(x = "rules"),
    function(x, value) {
        x@rhs <- value
        x
    })

## get the union of rhs and lhs
setMethod("items", signature(x = "rules"),
    function(x) {
        tmp <- x@lhs
        tmp@data <- as(x@lhs@data + x@rhs@data, "ngCMatrix")
        tmp
    })

# get the generating itemsets
setMethod("generatingItemsets", signature(x = "rules"),
    function(x) {
        new("itemsets", items = items(x), 
            quality = data.frame(support = quality(x)$support))
    })



##****************************************************
## subset, combine

setMethod("[", signature(x = "rules", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {           #]

        if (!missing(j)) 
            stop("incorrect dimension (j not possible)")
        if (missing(i)) return(x)
        slots <- intersect(slotNames(x), c("lhs", "rhs"))
        for (s in slots) 
            slot(x, s) <- slot(x, s)[i]
        x@quality <- x@quality[i,, drop = FALSE]
        x
    })

# fixme: the code above suggests it is possible that rhs
#        and/or lhs need not be present. however, the
#        code below does.


setMethod("c", signature(x = "rules"),
    function(x, ..., recursive = FALSE) {
#        args <- list(...)  
#        if(length(args) == 0) return(x)
#
#        # build quality data.frame first
#        # uses .combineQuality() defined in itemsets.R
#        q <- x@quality
#        for(i in 1:length(args)) {
#            q <- .combineQuality(q, args[[i]]@quality)
#        }
#
#
#        # create joint itemMatrix
#        lhs <- lapply(list(...), FUN = function(i) i@lhs)
#        rhs <- lapply(list(...), FUN = function(i) i@rhs)
#        new("rules", lhs = c(x@lhs, lhs, recursive = TRUE),
#            rhs = c(x@rhs, rhs, recursive = TRUE),
#            quality = q)

        args <- list(...)
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!inherits(y, "rules"))
                stop("can combine rules only")
            new("rules", lhs     = c(x@lhs, y@lhs), 
                         rhs     = c(x@rhs, y@rhs),
                         quality = .combineMeta(x, y, "quality"))
        }
        x
    })


## this utility function joins the lhs and rhs so it can be
## used for duplicated, unique, etc.
.joinedList <- function(x) {
    if (class(x) != "rules") return(NA)

    tmp <- LIST(x@lhs, decode = FALSE)
    rhs <- LIST(x@rhs, decode = FALSE)

    ## -> is used to distinguish between lhs and rhs
    for (i in 1:length(x)) 
        tmp[[i]] <- c(tmp[[i]], "->" , rhs[[i]])
    tmp
}


setMethod("duplicated", signature(x = "rules"),
    function(x, incomparables = FALSE, ...) {
        duplicated(.joinedList(x), incomparables = incomparables, ...)
    })


setMethod("match", signature(x = "rules", table = "rules"),
    function(x,  table, nomatch = NA, incomparables = FALSE) {
        match(.joinedList(x), .joinedList(table),
            nomatch = nomatch, incomparables = incomparables)
})


##************************************************
## summary


setMethod("summary", signature(object = "rules"), 
    function(object, ...) {
        sizes <- size(object@lhs) + size(object@rhs)
        
        new("summary.rules", 
            length = length(object),
            lengths = table(sizes),
            lengthSummary = summary(sizes),
            quality = if(length(object@quality) > 0) summary(object@quality)
            else summary(NULL))
    })

setMethod("show", signature(object = "summary.rules"), 
    function(object) {
        cat("set of", object@length, "rules\n\n")
        if(object@length>0) {
            cat("rule length distribution (lhs + rhs):")
            print(object@lengths)
            cat("\n")
            print(object@lengthSummary)
            cat("\nsummary of quality measures:\n")
            print(object@quality)
        }
    })


