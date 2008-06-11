##*******************************************************
## Class tidLists
##
## transaction ID lists


##*********************************************************
## dimensions of the binary matrix
setMethod("dim", signature(x = "tidLists"),
    function(x) rev(dim(x@data)))

## number of elements (rows)
setMethod("length", signature(x = "tidLists"),
    function(x) dim(x)[1])

## produces a vector of element sizes
setMethod("size", signature(x = "tidLists"),
    function(x) .Call("R_colSums_ngCMatrix", x@data))

##*******************************************************
## show/summary

setMethod("show", signature(object = "tidLists"),
    function(object) {
        cat("tidLists in sparse format with\n",
            dim(object)[1], "items/itemsets (rows) and\n",
            dim(object)[2], "transactions (columns)\n")
        invisible(NULL)
    }
)

setMethod("summary", signature(object = "tidLists"),
    function(object, maxsum = 6, ...) {
        tfs   <- sort(itemFrequency(object, type = "abs"), decreasing = TRUE)
        tsum  <- head(tfs, maxsum - as.integer(1))
        tsum  <- c(tsum, "(Other)" = sum(tfs) - sum(tsum))
        sizes <- size(object)

        new("summary.tidLists", 
            Dim                = dim(object),
            transactionSummary = tsum,
            lengths            = table(sizes),
            lengthSummary      = summary(sizes),
            itemInfo           = head(itemInfo(object), 3)
        )
    }
)

setMethod("show", signature(object = "summary.tidLists"),
    function(object) {
        cat("tidLists in sparse format with\n",
            object@Dim[1], "items/itemsets (rows) and\n",
            object@Dim[2], "transactions (columns)\n")

        cat("\nmost frequent transactions:\n")
        print(object@transactionSummary)
        cat("\nitem frequency distribution:\n")
        print(object@lengths)

        cat("\n")
        print(object@lengthSummary)

        if (length(names(object@itemInfo))) {
            cat("\nincludes extended item information - examples:\n")
            print(object@itemInfo)
        }
        invisible(NULL)
    }
)


## no t for associations
setMethod("t", signature(x = "tidLists"),
    function(x) stop("Object not transposable! Use as() for coercion to transactions."))

##*****************************************************
## subset

setMethod("[", signature(x = "tidLists", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {
        ## i and j are reversed
        if (!missing(i)) {
            if (is.character(i))
                i <- itemLabels(x) %in% i
            x@data <- .Call("R_colSubset_ngCMatrix", x@data, i)
            if (length(x@itemInfo))
                x@itemInfo <- x@itemInfo[i,, drop = FALSE]
        }
        if (!missing(j)) {
            x@data <- .Call("R_rowSubset_ngCMatrix", x@data, j)
            if (length(x@transactionInfo))
                x@transactionInfo <- x@transactionInfo[j,, drop = FALSE]
        }
        validObject(x, complete = TRUE)
        x
    }
)

##*****************************************************
## coercions 

setAs("tidLists", "list",
    function(from) LIST(from, decode = TRUE))

setMethod("LIST", signature(from = "tidLists"),
    function(from, decode = TRUE) {
        if (decode) {
            i <- from@transactionInfo[["transactionID"]]
            if (!is.null(i))
                i <- as.character(i)
            to <- .Call("R_asList_ngCMatrix", from@data, i)
            names(to) <- from@itemInfo[["labels"]]
            to
        } else
            .Call("R_asList_ngCMatrix", from@data, NULL)
    }
)

## dimnames<- is supposed to do the right thing!
setAs("tidLists", "matrix",
    function(from) {
        to <- as(t(from@data), "matrix")
        dimnames(to) <- 
            list(from@itemInfo[["labels"]], 
                 from@transactionInfo[["transactionID"]])
        to
    }
)

setAs("tidLists", "ngCMatrix",
    function(from) {
        dimnames(from@data) <- 
            list(from@transactionInfo[["transactionID"]],
                 from@itemInfo[["labels"]])
        from@data
    }
)

setAs("tidLists", "dgCMatrix",
    function(from) {
        to <- as(from@data, "dgCMatrix")
        dimnames(to) <- 
            list(from@transactionInfo[["transactionID"]], 
                 from@itemInfo[["labels"]])
        to
    }
)

setAs("tidLists", "transactions",
    function(from) 
        new("transactions", data            = t(from@data), 
                            itemInfo        = from@itemInfo, 
                            transactionInfo = from@transactionInfo))

setAs("transactions", "tidLists",
    function(from) 
        new("tidLists", data            = t(from@data), 
                        itemInfo        = from@itemInfo, 
                        transactionInfo = from@transactionInfo))

##
setAs("tidLists", "itemMatrix",
    function(from) {
        k <- match("transactionID", names(from@transactionInfo))
        if (!is.na(k))
            names(from@transactionInfo)[k] <- "itemsetID"

        new("itemMatrix", data        = t(from@data), 
                          itemInfo    = from@itemInfo,
                          itemsetInfo = from@transactionInfo)
    }
)

setAs("itemMatrix", "tidLists",
    function(from) {
        k <- match("itemsetID", names(from@itemsetInfo))
        if (!is.na(k))
            names(from@itemsetInfo)[k] <- "transactionID"

        new("tidLists", data            = t(from@data),
                        itemInfo        = from@itemInfo,
                        transactionInfo = from@itemsetInfo)
    }
)

##*****************************************************
## accessors

setMethod("transactionInfo", signature(x = "tidLists"),
    function(x) x@transactionInfo)

setMethod("itemInfo", signature(object = "tidLists"),
    function(object) object@itemInfo)

setMethod("itemLabels", signature(object = "tidLists"),
    function(object) as.character(object@itemInfo[["labels"]]))

setMethod("labels", signature(object = "tidLists"),
    function(object) {
        if (!length(i <- object@transactionInfo[["transactionID"]]))
            i <- seq_len(dim(object)[2])
        list(items = itemLabels(object), transactionID = as.character(i)) 
    }
)

###
