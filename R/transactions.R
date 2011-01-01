##*****************************************************
## Class transactions
##
## transaction data

##*****************************************************
## coercions

setAs("matrix", "transactions",
    function(from)
        new("transactions", as(from, "itemMatrix"), 
            transactionInfo = data.frame(transactionID = dimnames(from)[[1]])))

setAs("transactions", "matrix",
    function(from) {
        to <- as(as(from, "itemMatrix"), "matrix")
        if (length(i <- from@transactionInfo[["transactionID"]]))
            dimnames(to)[[1]] <- i
        to
    }
)

setAs("list", "transactions",
    function(from)
        new("transactions", as(from, "itemMatrix"), 
            transactionInfo = data.frame(transactionID = names(from))))

setAs("transactions", "list",
    function(from) LIST(from, decode = TRUE))

setMethod("LIST", signature(from = "transactions"),
    function(from, decode = TRUE) {
        to <- LIST(as(from, "itemMatrix"), decode)
        if (decode == TRUE) 
            names(to) <- from@transactionInfo[["transactionID"]]
        to
    }
)

## NOTES NA in levels is replaced by the string "NA"

setAs("data.frame", "transactions",
    function(from) {
        if (!length(from))
            return(new("transactions"))
        if (!all((p <- sapply(from, is.factor))))
            stop("column(s) ", paste(which(!p), collapse=", "), " not a factor")
        p <- seq(nrow(from))
        x <- lapply(from, function(x)
            tapply(p, x, eval, simplify = FALSE))

        l <- unlist(lapply(x, names), use.names = FALSE)
        l[is.na(l)] <- "NA"
        v <- rep(names(x), sapply(x, length))

        x <- unlist(x, recursive = FALSE, use.names = FALSE)
        p <- sapply(x, length)
        names(p) <- NULL
        x <- unlist(x, use.names = FALSE)

        x <- new("ngCMatrix", p   = c(0L, cumsum(p)),
                              i   = x - 1L,
                              Dim = c(dim(from)[1], length(p)))

        new("transactions",
            data     = t(x),
            itemInfo = data.frame(labels        = I(paste(v, l, sep = "=")),
                                  variables     = v,
                                  levels        = l),
            transactionInfo =
                       data.frame(transactionID = rownames(from)))
    }
)

## this does not reverse coercion data.frame -> transactions
## it is just used for output formating!
setAs("transactions", "data.frame",
    function(from) {
        if (!length(from)) 
            return (data.frame())

        items <- labels(as(from, "itemMatrix"))$elements

        if (!length(from@transactionInfo)) 
            return(data.frame(items = items))
        data.frame(transactionID = from@transactionInfo, items = items)
    }
)

## no t for associations
setMethod("t", signature(x = "transactions"),
    function(x) stop("Object not transposable! Use as(x, \"tidLists\") for coercion to tidLists."))

##*****************************************************
## subset + combine

## drop is unused
setMethod("[", signature(x = "transactions", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {
        ## i and j are reversed
        if (!missing(i)) {
            if (length(x@transactionInfo))
                x@transactionInfo <- x@transactionInfo[i,, drop = FALSE]
            x <- new("transactions", as(x, "itemMatrix")[i,, ..., drop],
                     transactionInfo = x@transactionInfo)
        }
        if (!missing(j))
            x <- new("transactions", as(x, "itemMatrix")[,j, ..., drop],
                     transactionInfo = x@transactionInfo)
        x
    }
)

setMethod("c", signature(x = "transactions"),
    function(x, ..., recursive = FALSE){
        args <- list(...)
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!is(y, "transactions"))
                stop("can only combine transactions")
            x <- new("transactions", c(as(x, "itemMatrix"), 
                                       as(y, "itemMatrix")),
                     transactionInfo = .combineMeta(x, y, "transactionInfo"))
        }
        x
    }
)

setMethod("merge", signature(x="transactions"),
    function(x, y, ...) {
	m <- merge(as(x, "itemMatrix"), as(y, "itemMatrix"))
	as(m, "transactions")
    })

##*****************************************************
## show / summary

setMethod("show", signature(object = "transactions"),
    function(object) {
        cat("transactions in sparse format with\n",
            length(object), "transactions (rows) and\n",
            nitems(object), "items (columns)\n")
        invisible(NULL)
    }
)

setMethod("summary", signature(object = "transactions"),
    function(object)
        new("summary.transactions",
            summary(as(object, "itemMatrix")),
            transactionInfo = head(object@transactionInfo, 3))
)

setMethod("show", signature(object = "summary.transactions"),
    function(object) {
        cat("transactions as ")
        show(as(object,"summary.itemMatrix"))

        if (length(object@transactionInfo)) {
            cat("\nincludes extended transaction information - examples:\n")
            print(object@transactionInfo)
        }
        invisible(NULL)
    }
)

##*****************************************************
## accessors

setMethod("transactionInfo", signature(x = "transactions"),
    function(x) x@transactionInfo)

setReplaceMethod("transactionInfo", signature(x = "transactions"),
    function(x, value) {
        x@transactionInfo <- value
        validObject(x)
        x
    }
)

setMethod("labels", signature(object = "transactions"),
    function(object) {
        ## make transaction labels
        if (!length(i <- object@transactionInfo[["transactionID"]]))
            i <- seq_len(length(object))
        list(items = itemLabels(object), transactionID = as.character(i))
    }
)

###
