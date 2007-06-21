##*******************************************************
## Class itemMatrix
##
## Basic class for sparse representation of sets or collections
## of itemsets


## dimensions of the binary matrix
setMethod("dim", signature(x = "itemMatrix"),
    function(x) rev(dim(x@data)))

## with dim nrow and ncol should work too

setMethod("nitems", signature(x = "itemMatrix"),
    function(x) dim(x)[2])

setMethod("length", signature(x = "itemMatrix"),
    function(x) dim(x)[1])

## produces a vector of element sizes
setMethod("size", signature(x = "itemMatrix"),
    function(x) {
        ## if Matrix had colSums implemented efficiently,
        ## we could use colSums(x). we use our own C code.
        ## however, diff(x@data@p) is nearly as fast.

        .Call("R_colSums_ngCMatrix", x@data)
    }
)

##*******************************************************
## Coercions

setAs("matrix", "itemMatrix",
    function(from) {
        ## Matrix requires logical. however, it should be
        ## the responsibility of mode to avoid uneccessary
        ## coercions.
        if (mode(from) != "logical") 
            mode(from) <- "logical"

        ## we have to transpose since there is currently no
        ## support for "ngRMatrix" in Matrix. note that we
        ## can fail later as the row or column names need 
        ## not neccessarily be unique.
        dn <- list(dimnames(from)[[1]], labels(from)[[2]])
        dimnames(from) <- NULL
        new("itemMatrix", 
            data        = t(as(from, "ngCMatrix")),
            itemInfo    = data.frame(labels    = I(dn[[2]])),
            itemsetInfo = data.frame(itemsetID =   dn[[1]]))
    }
)

setAs("itemMatrix", "matrix",
    function(from) {
        to <- as(t(from@data), "matrix")
        ## FIXME why coerce logical to integer?
        mode(to) <- "integer"
        
        ## due to a bug in Matrix the dimnames get lost
        dimnames(to) <- 
            list(from@itemsetInfo[["itemsetID"]], itemLabels(from))
        to
    }
)

setAs("itemMatrix", "list",
    function(from) LIST(from, decode = TRUE)
)

setMethod("LIST", signature(from = "itemMatrix"),
    function(from, decode = TRUE) {
        if (decode) {
            to <- .Call("R_asList_ngCMatrix", from@data, itemLabels(from))
            names(to) <- itemsetInfo(from)[["itemsetID"]]
            to
        } else
            .Call("R_asList_ngCMatrix", from@data, NULL)
    }
)

## note that stringsAsFactors works with NULL but
## I() does not as it coerces to list!

setAs("list", "itemMatrix", 
    function(from) {
        if (!length(from))
            return(new("itemMatrix"))
        if (!all(sapply(from, is.atomic)))
            stop("can coerce list with atomic components only")
        p <- sapply(from, length)
        names(p) <- NULL
        p <- cumsum(p)
        i <- unlist(from, use.names = FALSE)
        i <- factor(i)

        p <- new("ngCMatrix", p   = c(0L, p),
                              i   = c(i) - 1L,
                              Dim = c(length(levels(i)), length(p)))

        new("itemMatrix", 
            data        = p,
            itemInfo    = data.frame(labels    = I(levels(i))),
            itemsetInfo = data.frame(itemsetID = names(from)))
    }
)

## dimnanmes<-  is supposed to do the right thing.
## however, we should not provide these as long as
## handling of dimnames is broken in Matrix.
setAs("itemMatrix", "ngCMatrix",
    function(from) {
        dimnames(from@data) <- 
            list(from@itemInfo[["labels"]], from@itemsetInfo[["itemsetID"]])
        from@data
    }
)

setAs("itemMatrix", "dgCMatrix",
    function(from) {
        to <- as(from@data, "dgCMatrix")
        dimnames(to) <-
            list(from@itemInfo[["labels"]], from@itemsetInfo[["itemsetID"]])
        to
    }
)

##*******************************************************
## find elements which contain some items (as labels or 
## in itemInfo) note this is not what we would expect for
## %in% in R! but match below works the R-way

setMethod("%in%", signature(x = "itemMatrix", table = "character"),
    function(x, table) {
        pos <- match(table, itemLabels(x))
        if (any(is.na(pos)))
            stop("table contains an unknown item label" )
        size(x[, pos]) > 0
    }
)

## all items have to be in
setMethod("%ain%", signature(x = "itemMatrix", table = "character"),
    function(x, table) {
        pos <- match(table, itemLabels(x))
        if (any(is.na(pos))) 
            stop("table contains an unknown item label" )
        size(x[, pos]) == length(pos)
    }
)

## partial in  
setMethod("%pin%", signature(x = "itemMatrix", table = "character"),
    function(x, table) {
        if (length(table) > 1)
            stop(sQuote(table)," contains more than one item label pattern")
        pos <- grep(table, itemLabels(x))
        if (is.na(pos[1])) 
            return(rep(FALSE, length(x)))
        size(x[, pos]) > 0
    }
)

##*******************************************************
## subset, combine, duplicated, unique 

## remember that the sparse matrix is stored in transposed
## form (i and j are reversed). dimnames are handled by the
## C code but these should be list(NULL, NULL). extracting
## from data.frame() results in changing the dimension of 
## the data frame and must therfore be prevented. taking a
## row subset of a column sparse matrix is more costly than
## taking a column subset. for strict subsets it is more
## efficient to do the latter first.

setMethod("[", signature(x = "itemMatrix", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {
        if (!missing(i)) {
            x@data <- .Call("R_colSubset_ngCMatrix", x@data, i)
            if (length(x@itemsetInfo))
                x@itemsetInfo <- x@itemsetInfo[i,, drop = FALSE]
        }
        if (!missing(j)) {
            ## fixed thanks to a bug report by Seth Falcon (06/31/01)
            if (is.character(j)) 
                j <- itemLabels(x) %in% j
            x@data <- .Call("R_rowSubset_ngCMatrix", x@data, j)
            if (length(x@itemInfo))
                x@itemInfo <- x@itemInfo[j,, drop = FALSE]
        }
        validObject(x, complete = TRUE)
        x
    }
)

## fixme: labels are not sorted
setMethod("c", signature(x = "itemMatrix"),
    function(x, ..., recursive = FALSE) {
        args <- list(...)
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!is(y, "itemMatrix"))
                stop("can only combine itemMatrix")
            x@itemsetInfo <- .combineMeta(x, y, "itemsetInfo")
            k <- match(itemLabels(y), itemLabels(x))
            n <- which(is.na(k))
            if (length(n)) {
                k[n] <- x@data@Dim[1] + seq(length(n))
                x@data@Dim[1] <- x@data@Dim[1] + length(n)
                x@itemInfo <- rbind(x@itemInfo, 
                                    y@itemInfo[n,, drop = FALSE])
            }
            if (any(k != seq_len(length(k))))
                y@data <- .Call("R_recode_ngCMatrix", y@data, k)
            if (y@data@Dim[1] <  x@data@Dim[1])
                y@data@Dim[1] <- x@data@Dim[1]
            x@data <- .Call("R_cbind_ngCMatrix", x@data, y@data)
        }
        validObject(x, complete = TRUE)
        x
    }
)

setMethod("duplicated", signature(x = "itemMatrix"),
    function(x, incomparables = FALSE) {
        i <- .Call("R_pnindex", x@data, NULL, FALSE)
        duplicated(i)
    }
)

setMethod("unique", signature(x = "itemMatrix"),
    function(x,  incomparables = FALSE)
        x[!duplicated(x, incomparables = incomparables)])

## checks if the item labels conform
## and uses more efficient C code
setMethod("match", signature(x = "itemMatrix", table = "itemMatrix"),
    function(x, table, nomatch = NA, incomparables = FALSE) {
        k <- match(itemLabels(x), itemLabels(table))
        n <- which(is.na(k))
        if (length(n)) {
            k[n] <- table@data@Dim[1] + seq(length(n))
            table@data@Dim[1] <- table@data@Dim[1] + length(n)
        }
        if (any(k != seq_len(length(k))))
            x@data <- .Call("R_recode_ngCMatrix", x@data, k)
        if (x@data@Dim[1] <  table@data@Dim[1])
            x@data@Dim[1] <- table@data@Dim[1]
        i <- .Call("R_pnindex", table@data, x@data, FALSE)
        match(i, seq(length(table)), nomatch = nomatch, 
                                     incomparables = incomparables)
    }
)

##*******************************************************
## accessors

## fixme

setMethod("labels", signature(object = "itemMatrix"),
    function(object, itemSep = ",", setStart = "{", setEnd = "}")
        list(items    = itemLabels(object),
             elements = paste(setStart, sapply(as(object, "list"), paste,
                              collapse = itemSep), setEnd, sep = "")))

setMethod("itemLabels", signature(object = "itemMatrix"),
    function(object) as.character(object@itemInfo[["labels"]]))

setReplaceMethod("itemLabels", signature(object = "itemMatrix"),
    function(object, value) {
        object@itemInfo[["labels"]] <- as.character(value)
        validObject(object)
        object
    }
)

setMethod("itemInfo", signature(object = "itemMatrix"),
    function(object) object@itemInfo)

setReplaceMethod("itemInfo", signature(object = "itemMatrix"),
    function(object, value) {
        object@itemInfo <- value
        validObject(object)
        object
    }
)

setMethod("itemsetInfo", signature(object = "itemMatrix"),
    function(object) object@itemsetInfo)

setReplaceMethod("itemsetInfo", signature(object = "itemMatrix"),
    function(object, value) {
        object@itemsetInfo <- value
        validObject(object)
        object
    }
)

##*******************************************************
## show/summary + plots

setMethod("show", signature(object = "itemMatrix"),
    function(object) {
        cat("itemMatrix in sparse format with\n", 
            length(object), "rows (elements/transactions) and\n", 
            nitems(object), "columns (items)\n")
    }
)

## NOTE Matrix coerces to dgTMatrix and the method for
##      this class uses levelplot

setMethod("image", signature(x = "itemMatrix"),
    function(x, xlab = "Items (Columns)", ylab = "Elements (Rows)", ...) {
        ## due to a bug somewhere we have to use this hack
        x <- as(as(t(x@data), "dgCMatrix"), "dgTMatrix")
        image(x, sub = NULL, ylab = ylab, xlab = xlab, colorkey = FALSE, ...)
    }
)

setMethod("summary", signature(object = "itemMatrix"),
    function(object, maxsum = 6, ...) {
        ifs   <- sort(itemFrequency(object, type = "abs"), decreasing = TRUE)
        isum  <- head(ifs, maxsum - 1L)
        isum  <- c(isum, "(Other)" = sum(ifs) - sum(isum))
        sizes <- size(object)
            
        new("summary.itemMatrix", 
            Dim           = dim(object),
            itemSummary   = isum,
            lengths       = table(sizes),
            lengthSummary = summary(sizes),
            itemInfo      = head(object@itemInfo, 3)
        )
    }
)

setMethod("show", signature(object = "summary.itemMatrix"),
    function(object) {
        cat("itemMatrix in sparse format with\n",
            object@Dim[1], "rows (elements/itemsets/transactions) and\n",
            object@Dim[2], "columns (items)\n")

        cat("\nmost frequent items:\n")
        print(object@itemSummary)
        cat("\nelement (itemset/transaction) length distribution:\n")
        print(object@lengths)

        cat("\n")
        print(object@lengthSummary)

        if (length(names(object@itemInfo))) {
            cat("\nincludes extended item information - examples:\n")
            print(object@itemInfo)
        }
    }
)

## removed unused ... from function arguments [ceeboo 2007]

###
