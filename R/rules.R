##*******************************************************
## Class rules
##
## a set of rules, subclass of associations


##************************************************
## dimensions
setMethod("length", signature(x = "rules"),
    function(x) length(x@lhs))

setMethod("size", signature(x = "rules"),
    function(x) size(x@lhs) + size(x@rhs))

##***********************************************
## coercion
setAs("rules", "data.frame",
    function(from) {
        if (!length(from)) 
            return (data.frame())
        if (!length(from@quality)) 
            return(data.frame(rules = labels(from)))
        data.frame(rules = labels(from), from@quality)
    }
)

##***********************************************
## labels

setMethod("labels", signature(object = "rules"),
    function(object, ruleSep = " => ", ...)
        paste(labels(object@lhs, ...)$elements, ruleSep,
              labels(object@rhs, ...)$elements, sep = ""))

setMethod("itemLabels", signature(object = "rules"),
    function(object)itemLabels(lhs(object)))

##************************************************
## accessors

setMethod("itemInfo", signature(object = "rules"),
    function(object) itemInfo(object@lhs))

setMethod("lhs", signature(x = "rules"),
    function(x) x@lhs)

setReplaceMethod("lhs", signature(x = "rules"),
    function(x, value) {
        x@lhs <- value
        validObject(x)
        x
    }
)

setMethod("rhs", signature(x = "rules"),
    function(x) x@rhs)

setReplaceMethod("rhs", signature(x = "rules"),
    function(x, value) {
        x@rhs <- value
        validObject(x)
        x
    }
)

## get the union of rhs and lhs
setMethod("items", signature(x = "rules"),
    function(x) {
        x@lhs@data <- .Call("R_or_ngCMatrix", x@lhs@data, x@rhs@data)
        x@lhs
    }
)

# get the generating itemsets
setMethod("generatingItemsets", signature(x = "rules"),
    function(x)
        new("itemsets", 
            items   = items(x), 
            quality = data.frame(support = x@quality[["support"]])))

##****************************************************
## subset, combine

setMethod("[", signature(x = "rules", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {
        if (!missing(j)) 
            stop("incorrect dimension (j not possible)")
        if (missing(i))
            return(x)
        slots <- intersect(slotNames(x), c("lhs", "rhs"))
        for (s in slots) 
            slot(x, s) <- slot(x, s)[i]
        if (length(x@quality))
            x@quality <- x@quality[i,, drop = FALSE]
        validObject(x)
        x
    }
)

# fixme: the code above suggests it is possible that rhs
#        and/or lhs need not be present. however, the
#        code below does.

setMethod("c", signature(x = "rules"),
    function(x, ..., recursive = FALSE) {
        args <- list(...)
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!is(y, "rules"))
                stop("can combine rules only")
            new("rules", lhs     = c(x@lhs, y@lhs), 
                         rhs     = c(x@rhs, y@rhs),
                         quality = .combineMeta(x, y, "quality"))
        }
        x
    }
)

## this utility function joins the lhs and rhs so it can be
## used for duplicated, unique, etc. 0 is used as separator
## which avoids coercion to character.
.joinedList <- function(x) {
    if (class(x) != "rules")
        stop("not of class rules")

    mapply(function(l, r) c(l, 0, r),
           LIST(x@lhs, decode = FALSE),
           LIST(x@rhs, decode = FALSE), SIMPLIFY = FALSE)
}

setMethod("duplicated", signature(x = "rules"),
    function(x, incomparables = FALSE)
        duplicated(.joinedList(x), incomparables = incomparables))

setMethod("match", signature(x = "rules", table = "rules"),
    function(x, table, nomatch = NA_integer_, incomparables = NULL)
        match(.joinedList(x), .joinedList(table),
              nomatch = nomatch, incomparables = incomparables))

##************************************************
## summary

setMethod("summary", signature(object = "rules"), 
    function(object, ...) {
        sizes <- size(object@lhs) + size(object@rhs)
        
        new("summary.rules", 
            length        = length(object),
            lengths       = table(sizes),
            lengthSummary = summary(sizes),
            quality       = 
                if (length(object@quality)) summary(object@quality)
                else                        summary(NULL))
    }
)

setMethod("show", signature(object = "summary.rules"), 
    function(object) {
        cat("set of", object@length, "rules\n\n")
        if(object@length) {
            cat("rule length distribution (lhs + rhs):")
            print(object@lengths)

            cat("\n")
            print(object@lengthSummary)

            cat("\nsummary of quality measures:\n")
            print(object@quality)
        }
    }
)

###
