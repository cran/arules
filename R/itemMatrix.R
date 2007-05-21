##*******************************************************
## Class itemMatrix
##
## Basic class for sparse representation of sets or collections
## of itemsets


## dimensions of the binary matrix
setMethod("dim", signature(x = "itemMatrix"),
    function(x) rev(dim(x@data))
)
## with dim nrow and ncol should work too


setMethod("nitems", signature(x = "itemMatrix"),
    function(x, ...) dim(x)[2]
)

setMethod("length", signature(x = "itemMatrix"),
    function(x) dim(x)[1]
)

## produces a vector of element sizes
setMethod("size", signature(x = "itemMatrix"),
    function(x, ...) {
        ## if Matrix had colSums implemented efficiently, we could do
        ## colSums(x)
        ## a possible hack would be diff(x@data@p)

        ## we use our own C code
        .Call("R_colSums_ngCMatrix", x@data)
    })


##*******************************************************
## Coercions

setAs("matrix", "itemMatrix",
    function(from) {

        ## make logical
        if(mode(from) != "logical") mode(from) <- "logical"

        ## we have to transpose since there is 
        ## currently only minimal support for "ngRMatrix" in Matrix
        i <- t(as(from, "ngCMatrix"))
        ## kill Dimnames
        i@Dimnames <- list(NULL, NULL)

        new("itemMatrix", data = i,  
            itemInfo = data.frame(labels = I(as.character(labels(from)[[2]]))))
    })

setAs("itemMatrix", "matrix",
    function(from) {

        m <- as(t(from@data), "matrix")
        mode(m) <- "integer"
        
        ## FIXME:
        ## matrix looses dimnames during coercion in Matrix!
        dimnames(m)[[2]] <- itemLabels(from)
        m
    })

setAs("itemMatrix", "list",
    function(from) LIST(from, decode = TRUE)
)

setMethod("LIST", signature(from = "itemMatrix"),
    function(from, decode = TRUE) {
        z <- as(from@data, "list")
        if (decode == TRUE ) return(decode(z, itemLabels(from)))
        else return(z)
    })


setAs("list","itemMatrix", function(from, to) {

        ## get names
        from_names <- unique(unlist(from))
        data <- c(1:(length(from_names)))
        names(data) <- from_names 

        ## code items from 1..numItems and kill doubles
        l <- lapply(from, function(x) unique(as.vector(data[as.character(x)])))
        l <- as(l, "ngCMatrix")
        ## kill Dimnames
        l@Dimnames <- list(NULL, NULL)

        new("itemMatrix", data = l, 
            itemInfo = data.frame(labels = I(as.character(from_names))))
    })

## beware: "ngCMatrix" and "dgCMatrix" is transposed!
setAs("itemMatrix", "ngCMatrix",
    function(from) {
        ngC <- from@data
        ## this is low level since ngCMatrix has no labels <- 
        ngC@Dimnames[[1]] <- itemLabels(from)
        ngC
    })

setAs("itemMatrix", "dgCMatrix",
    function(from) as(as(from, "ngCMatrix"), "dgCMatrix")
)

##*******************************************************
## find elements which contain some items (as 
##        labels or in itemInfo)
## note this is not what we would expect for %in% in R!
## but match below works the R-way
setMethod("%in%", signature(x = "itemMatrix", table = "character"),
    function(x, table) {
        pos <- match(table, itemLabels(x))
        if(any(is.na(pos))) stop("table contains an unknown item label" )
        return(size(x[, pos]) > 0)
    })

## all items have to be in
setMethod("%ain%", signature(x = "itemMatrix", table = "character"),
    function(x, table) {
        pos <- match(table, itemLabels(x))
        if(any(is.na(pos))) stop("table contains an unknown item label" )
        return(size(x[, pos]) == length(pos))
    })

## partial in  
setMethod("%pin%", signature(x = "itemMatrix", table = "character"),
    function(x, table) {
        if (length(table) > 1)
            stop(sQuote(table)," contains more than one item label pattern")
        pos <- grep(table, itemLabels(x))
        if(is.na(pos[1])) return(rep(FALSE, length(x)))
        return(size(x[, pos]) > 0)
    })


##*******************************************************
## subset, combine, duplicated, unique 

## remember the sparse matrix is stored in transposed form (i <-> j)
setMethod("[", signature(x = "itemMatrix", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {  #]

        ## the code in Matrix does not work with multindexes
        ## use was x@data[j, ..., drop = FALSE] with Matrix
        ## i and j are reversed!
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
            x@itemInfo <- x@itemInfo[j,, drop = FALSE]
        }

        x
    })


#setMethod("c", signature(x = "itemMatrix"),
#    function(x, ..., recursive = FALSE){
#
#        z <- list(...)
#        if(length(z) < 1) return(x) 
#
#        if(recursive) z <- unlist(z)
#        if(any(!sapply(is (z, "itemMatrix")))) 
#            stop("All elements have to be itemMatrix!")
#
#        num_items <- x@data@Dim[1]
#        num_trans <- x@data@Dim[2]
#        i <- x@data@i
#        p <- x@data@p
#        ## pmax makes sure that the column counts for the added elements
#        # start with the number of the previous element
#        pmax <- p
#
#        for (elem in z) {
#            if(elem@data@Dim[1] != num_items) 
#            stop ("Number of items mismatch")
#            pmax <-  p[length(p)]
#
#            i <- c(i, elem@data@i)
#            p <- c(p, (elem@data@p[-1] + pmax))
#            num_trans <- num_trans + elem@data@Dim[2]
#        }
#
#        data <- new("ngCMatrix", i = i, p = p, 
#            Dim = c(num_items,num_trans) )
#
#        new("itemMatrix", 
#            data = new("ngCMatrix", i = i, p = p, 
#                Dim = c(num_items,num_trans)), 
#            itemInfo = z[[1]]@itemInfo)
#    })

setMethod("c", signature(x = "itemMatrix"),
    function(x, ..., recursive = FALSE) {
        args <- list(...)
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!inherits(y, "itemMatrix"))
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
            if (any(k != seq(length(k))))
                y@data <- .Call("R_recode_ngCMatrix", y@data, k)
            if (y@data@Dim[1] <  x@data@Dim[1])
                y@data@Dim[1] <- x@data@Dim[1]
            x@data <- .Call("R_cbind_ngCMatrix", x@data, y@data)
        }
        validObject(x)
        x
    })

setMethod("duplicated", signature(x = "itemMatrix"),
    function(x, incomparables = FALSE, ...) {
        #duplicated(LIST(x, decode  FALSE),
        #    incomparables = incomparables, ...)
        
        # more efficient C code
        i <- .Call("R_pnindex", x@data, NULL, FALSE)
        duplicated(i)
    })

setMethod("unique", signature(x = "itemMatrix"),
    function(x,  incomparables = FALSE, ...) {
        x[!duplicated(x, incomparables = incomparables, ...)]
    })

#setMethod("match", signature(x = "itemMatrix", table = "itemMatrix"),
#    function(x,  table, nomatch = NA, incomparables = FALSE) {
#        match(LIST(x, decode = FALSE), LIST(table, decode = FALSE), 
#            nomatch = nomatch, incomparables = incomparables)
#    })

## more efficient C code
setMethod("match", signature(x = "itemMatrix", table = "itemMatrix"),
    function(x, table, nomatch = NA, incomparables = FALSE) {
        k <- match(itemLabels(x), itemLabels(table))
        n <- which(is.na(k))
        if (length(n)) {
            k[n] <- table@data@Dim[1] + seq(length(n))
            table@data@Dim[1] <- table@data@Dim[1] + length(n)
        }
        if (any(k != seq(length(k))))
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

setMethod("labels", signature(object = "itemMatrix"),
    function(object, itemSep = ", ", setStart = "{", setEnd = "}", ...) {
        list(items = as.character(object@itemInfo[["labels"]]),
            elements = paste(setStart, sapply(as(object, "list"),
                    function(x) paste(x, collapse =itemSep)), setEnd, sep=""))
    })

setMethod("itemLabels", signature(object = "itemMatrix"),
    function(object, ...) as.character(object@itemInfo[["labels"]])
)

setReplaceMethod("itemLabels", signature(object = "itemMatrix"),
    function(object, value) {
        if(length(value) != nitems(object)) 
        stop("number of itemLabels does not match number of items")
       
        object@itemInfo[["labels"]] <- as.character(value)
        object
    })

setMethod("itemInfo", signature(object = "itemMatrix"),
    function(object) object@itemInfo
)

setReplaceMethod("itemInfo", signature(object = "itemMatrix"),
    function(object, value) {
        if(nrow(value) != nitems(object))
        stop("number of entries in itemInfo does not match number of items")

        object@itemInfo <- value
        object
})


setMethod("itemsetInfo", signature(object = "itemMatrix"),
    function(object) object@itemsetInfo
)

setReplaceMethod("itemsetInfo", signature(object = "itemMatrix"),
    function(object, value) {
        if(nrow(value) != length(object))
        stop("number of entries in itemsetInfo does not match number of rows")

        object@itemsetInfo <- value
        object
    })


##*******************************************************
## show/summary + plots

setMethod("show", signature(object = "itemMatrix"),
    function(object) {
        cat("itemMatrix in sparse format with\n", 
            length(object), "rows (elements/transactions) and\n", 
            nitems(object), "columns (items)\n")
        invisible(object)
    })


setMethod("image", signature(x = "itemMatrix"),
    function(x, 
        xlab = "Items (Columns)", 
        ylab = "Elements (Rows)", ...) {
        
        ## FIXME:
        ## there is something weired with Matrix
        ## why not just i <- t(x@data) ???
        i <- as(as(t(x@data), "dgCMatrix"), "dgTMatrix")
        image(i, sub = NULL, ylab = ylab, xlab = xlab, 
            colorkey = FALSE, ...)
    })


setMethod("summary", signature(object = "itemMatrix"),
    function(object, maxsum = 6, ...) {
        ifs <- sort(itemFrequency(object, type = "abs"), decreasing = TRUE)
        isum <- head(ifs, maxsum - as.integer(1))
        isum <- c(isum, "(Other)" = sum(ifs) - sum(isum))
        sizes <- size(object)
            
        new("summary.itemMatrix", Dim=dim(object),
            itemSummary = isum,
            lengths = table(sizes),
            lengthSummary = summary(sizes),
            itemInfo= head(itemInfo(object), 3)
        )
    })

setMethod("show", signature(object = "summary.itemMatrix"),
    function(object) {
        cat("itemMatrix in sparse format with\n",
            object@Dim[1],"rows (elements/itemsets/transactions) and\n",
            object@Dim[2],"columns (items)\n")

        cat("\nmost frequent items:\n")
        print(object@itemSummary)
        cat("\nelement (itemset/transaction) length distribution:")
        print(object@lengths)

        cat("\n")
        print(object@lengthSummary)

        if(length(names(object@itemInfo)) > 1) {
            cat("\nincludes extended item information - examples:\n")
            print(object@itemInfo)}
    })



