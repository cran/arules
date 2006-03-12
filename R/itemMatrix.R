##*******************************************************
## Class itemMatrix
##
## Basic class for sparse representation of sets or collections
## of itemsets


## dimensions of the binary matrix
setMethod("dim", signature(x = "itemMatrix"),
    function(x) rev(dim(x@data))
)

setMethod("nitems", signature(x = "itemMatrix"),
    function(x) dim(x)[2]
)

setMethod("length", signature(x = "itemMatrix"),
    function(x) dim(x)[1]
)

## produces a vector of element sizes
setMethod("size", signature(x = "itemMatrix"),
    function(x) {
        ## FIXME:
        ## if Matrix had colSums implemented efficiently, we could do
        ## colSums(x)

        ## a possible hack would be diff(x@data@p)

        ## for now we use our own C code
        .Call("R_colSums_ngCMatrix", x@data)
    })


##*******************************************************
## Coercions

setAs("matrix", "itemMatrix",
    function(from) {

        ## make logical
        if(storage.mode(from) != "logical") storage.mode(from) <- "logical"

        ## we have to transpose since there is not enough support 
        ## for "ngRMatrix"
        i <- t(as(from, "ngCMatrix"))

        new("itemMatrix", data = i,  
            itemInfo = data.frame(labels = labels(from)[[2]]))
    })

setAs("itemMatrix", "matrix",
    function(from) {

        m <- as(t(from@data), "matrix")
        storage.mode(m) <- "integer"
        
        ## FIXME:
        ## something -> matrix looses dimnames in Matrix!
        dimnames(m)[[2]] <- from@itemInfo[["labels"]]
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

        new("itemMatrix", data= as(l, "ngCMatrix"), 
            itemInfo = data.frame(labels = from_names))
    })

setAs("itemMatrix", "ngCMatrix",
    function(from) from@data
)

setAs("itemMatrix", "dgCMatrix",
    function(from) as(from@data, "dgCMatrix")
)

##*******************************************************
## find elements which contain some items (as 
##        labels or in itemInfo)
## note this is not what we would expect for %in% in R!
## but match below works the R-way
setMethod("%in%", signature(x = "itemMatrix"),
    function(x, table) {
        pos <- match(table, itemLabels(x))
        if(is.na(pos[1])) return(rep(FALSE, length(x)))
        return(size(x[, pos]) > 0)
    })

## partial in  
setMethod("%pin%", signature(x = "itemMatrix"),
    function(x, table) {
        pos <- grep(table, itemLabels(x))
        if(is.na(pos[1])) return(rep(FALSE, length(x)))
        return(size(x[, pos]) > 0)
    })


##*******************************************************
## subset, combine, duplicated, unique 

## remember the sparce matrix is tored in transposed form (i <-> j)
setMethod("[", signature(x = "itemMatrix", i = "ANY", j = "ANY", drop = "ANY"),
        function(x, i, j, ..., drop) {  #]

        if(missing(j) && missing(i)) return(x)

        ## drop is always false
        drop <- FALSE 

        ## fixed thanks to a bug report by Seth Falcon (06/31/01)
        if(!missing(j)){
            if(is.character(j)) j <- itemLabels(x) %in% j
            x@itemInfo <- x@itemInfo[j, , drop = FALSE]
        }
    
        ## i and j are reversed!
        if(missing(i)) x@data <- x@data[j, ..., drop = drop]
        else if(missing(j)) x@data <- x@data[,i, ..., drop = drop]
        else x@data <- x@data[j,i, ..., drop = drop]
        
        x
    })



setMethod("c", signature(x = "itemMatrix"),
    function(x, ..., recursive = FALSE){

        z <- list(...)

        if(recursive == TRUE) {
            flatten <- function(x) {
                res <- list()

                for (i in 1 : length(x)) {
                    if(class(x[[i]]) == "list") 
                    res <- c(res, Recall(x[[i]]))
                    else res <- c(res, list(x[[i]]))
                }

                return(res)
            }

            z <- flatten(list(...)) 
        } 

        if(length(z) < 1) return(x) 

        num_items <- x@data@Dim[1]
        num_trans <- x@data@Dim[2]
        i <- x@data@i
        p <- x@data@p
        ## pmax makes sure that the column counts for the added elements
        # start with the number of the previous element
        pmax <- p

        for (elem in z) {
            if(elem@data@Dim[1] != num_items) 
            stop ("Number of items mismatch")
            pmax <-  p[length(p)]

            i <- c(i, elem@data@i)
            p <- c(p, (elem@data@p[-1] + pmax))
            num_trans <- num_trans + elem@data@Dim[2]
        }

        data <- new("ngCMatrix", i = i, p = p, 
            Dim = c(num_items,num_trans) )

        new("itemMatrix", 
            data = new("ngCMatrix", i = i, p = p, 
                Dim = c(num_items,num_trans)), 
            itemInfo = z[[1]]@itemInfo)
    })

setMethod("duplicated", signature(x = "itemMatrix"),
    function(x, incomparables = FALSE, ...) {
        #duplicated(LIST(x, decode = FALSE),
        #    incomparables = incomparables, ...)
        i <- .Call("R_pnindex", x@data, FALSE)
        duplicated(i)
    })

setMethod("unique", signature(x = "itemMatrix"),
    function(x,  incomparables = FALSE, ...) {
        x[!duplicated(x, incomparables = incomparables, ...)]
    })

setMethod("match", signature(x = "itemMatrix", table = "itemMatrix"),
    function(x,  table, nomatch = NA, incomparables = FALSE) {
        #match(LIST(x, decode = FALSE), LIST(table, decode = FALSE), 
        #    nomatch = nomatch, incomparables = incomparables)
        comb <- c(x, table)    
        i <- .Call("R_pnindex", comb@data, FALSE)
        match(i[c(1:length(x))], i[-c(1:length(x))], 
            nomatch = nomatch, incomparables = incomparables)
})



##*******************************************************
## accessors

setMethod("labels", signature(object = "itemMatrix"),
    function(object, itemSep = ", ", setStart = "{", setEnd = "}", ...) {
        list(items = as(object@itemInfo[["labels"]], "character"),
            elements = paste(setStart, sapply(as(object, "list"),
                    function(x) paste(x, collapse =itemSep)), setEnd, sep=""))
    })

setMethod("itemLabels", signature(object = "itemMatrix"),
    function(object, ...) as(object@itemInfo[["labels"]], "character")
)

setReplaceMethod("itemLabels", signature(object = "itemMatrix"),
    function(object, value) {
        if(length(value) != nitems(object)) 
        stop("number of itemLabels does not match number of items")
        
        object@itemInfo <- data.frame(labels = value)
        ## this is low level since ngCMatrix has no labels <- 
        object@data@Dimnames[[1]] <- value
        object
    })

setMethod("itemInfo", signature(object = "itemMatrix"),
    function(object) object@itemInfo
)

setReplaceMethod("itemInfo", signature(object = "itemMatrix"),
    function(object, value) {
        if(dim(value)[1] != nitems(object))
        stop("number of entries in itemInfo does not match number of items")

        object@itemInfo <- value
        object@data@Dimnames[[1]] <- value[,1]
        object
    })


##*******************************************************
## show/summary + plots

setMethod("show", signature(object = "itemMatrix"),
    function(object) {
        cat("itemMatrix in sparse format with\n", 
            dim(object)[1],"rows (elements/transactions) and\n", 
            dim(object)[2],"columns (items)\n")
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
        lst <- as(object,"list")
        chars <- as(unlist(lst), "character")
        new("summary.itemMatrix", Dim=dim(object),
            itemSummary = summary(as.factor(chars), maxsum = maxsum),
            lengths = table(size(object)),
            lengthSummary = summary(size(object)),
            itemInfo=itemInfo(object)[1:min(3, length(labels(object))),, 
                drop = FALSE])
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



