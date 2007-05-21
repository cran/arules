##*****************************************************
## Class transactions
##
## transaction data

##*****************************************************
## coercions

setAs("matrix", "transactions",
    function(from) {
        new("transactions", as(from, "itemMatrix"), 
            transactionInfo = data.frame(transactionID = dimnames(from)[[1]]))
    })

setAs("transactions", "matrix",
    function(from) {
        m <- as(as(from, "itemMatrix"), "matrix")
        if (!is.null(from@transactionInfo[["transactionID"]])) 
        dimnames(m)[[1]] <- from@transactionInfo[["transactionID"]]
        return(m)
    })


setAs("list", "transactions",
    function(from) {
        new("transactions", as(from, "itemMatrix"), 
            transactionInfo = data.frame(transactionID = names(from)))
    })

setAs("transactions", "list",
    function(from) LIST(from, decode = TRUE)
)

setMethod("LIST", signature(from = "transactions"),
    function(from, decode = TRUE) {
        l <- LIST(as(from, "itemMatrix"), decode)
        if(decode == TRUE) 
        names(l) <- from@transactionInfo[["transactionID"]]
        l
    })

setAs("data.frame", "transactions", function(from) {
        if (!all(sapply(from, is, "factor")))
        stop("Column ", names(which(!sapply(from, is, "factor"))),
            " is not a factor.")

        from_levels <- sapply(from, levels, simplify = FALSE)
        assign      <- sapply(from_levels, length, USE.NAMES = FALSE)
        to_levels   <- unlist(from_levels, use.names = FALSE)
        to_vars     <- rep(names(from), assign)
        to_labels   <- paste(to_vars, to_levels, sep = "=")

        lev         <- c(0, cumsum(assign))
        to_dim      <- c(length(to_labels),dim(from)[1])
        len         <- rep(dim(from)[2], to_dim[2])
        v           <- lapply(1:dim(from)[2], function(i) factor(from[[i]],
                levels = levels(from[[i]]),
                labels = lev[i]:(lev[i+1]-1)))
        v           <- data.frame(v)
        i           <- as.integer(t(v))

        if (any(is.na(v))) {
            i <- i[!is.na(i)]
            ind <- table(which(is.na(v), arr.ind = TRUE)[,1])
            rowsNA <- as.integer(names(ind))
            len[rowsNA] <- len[rowsNA] - ind
        }

        p <- as.integer(c(0, cumsum(len)))
        z <- new("ngCMatrix", i = i, p = p, Dim = to_dim)
        ##cat("Recoded",dim(from)[2],"variables to",to_dim[1],"binary items\n")
        new("transactions", new("itemMatrix", data = z,
                itemInfo = data.frame(labels = I(as.character(to_labels)), 
                    variables = to_vars, levels = to_levels)))
    })


## this does not reverse coercion data.frame -> transactions
## it is just used for output formating!
setAs( "transactions", "data.frame", function(from) {
        if(!length(from)) return (data.frame())

        items <- paste("{",sapply(as(from, "list"),
                    function(x) paste(x, collapse =", ")),"}", sep="")

        if(!length(from@transactionInfo)) return(data.frame(items = items))
        data.frame(items = items, from@transactionInfo)


    })

## no t for associations
setMethod("t", signature(x = "transactions"),
    function(x)
        stop("Object not transposable! Use as() for coercion to tidLists.")
    )

##*****************************************************
## subset + combine

setMethod("[", signature(x = "transactions",            # ] 
        i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {

        if(missing(j) && missing(i)) return(x)

        if(missing(i)) {
            new("transactions", as(x, "itemMatrix")[, j,..., drop = FALSE],
                transactionInfo = x@transactionInfo)
        }else if(missing(j)) {
            new("transactions", as(x, "itemMatrix")[i,,..., drop = FALSE],
                transactionInfo = x@transactionInfo[i,, drop = FALSE])
        }else{
            new("transactions", as(x, "itemMatrix")[i, j,..., drop = FALSE],
                transactionInfo = x@transactionInfo[i,, drop = FALSE])
        }
    })

setMethod("c", signature(x = "transactions"),
    function(x, ..., recursive = FALSE){

        args <- list(...)
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!inherits(y, "transactions"))
                stop("can only combine transactions")
            x <- new("transactions", c(as(x, "itemMatrix"), 
                                       as(y, "itemMatrix")),
                     transactionInfo = .combineMeta(x, y, "transactionInfo"))
        }
        x
    })


##*****************************************************
## show / summary


setMethod("show", signature(object = "transactions"),
    function(object) {
        cat("transactions in sparse format with\n",
            length(object), "transactions (rows) and\n",
            nitems(object), "items (columns)\n")
    })

setMethod("image", signature(x = "transactions"),
    function(x, ...)
    image(as(x, "itemMatrix"), xlab = "Items (Columns)", 
        ylab = "Transactions (Rows)")
)

setMethod("summary", signature(object = "transactions"),
    function(object, ...)
    new("summary.transactions", summary(as(object,"itemMatrix")),
        transactionInfo = head(transactionInfo(object), 3)
    )
)

setMethod("show", signature(object = "summary.transactions"),
    function(object) {
        cat("transactions as ")
        show(as(object,"summary.itemMatrix"))

        if(length(names(object@transactionInfo)) > 0) {
            cat("\nincludes extended transaction information - examples:\n")
            print(object@transactionInfo)}
    })



##*****************************************************
## accessors


setMethod("transactionInfo", signature(x = "transactions"),
    function(x) x@transactionInfo
)

setReplaceMethod("transactionInfo", signature(x = "transactions"),
    function(x, value) {
        x@transactionInfo <- value
        x
    })

setMethod("labels", signature(object = "transactions"),
    function(object, ...) {

        ## check if transaction labels exist
        transactionID <- 
        if(length(object@transactionInfo) != 0)   
            as(object@transactionInfo[["transactionID"]], "character")
        else as(1 : length(object), "character")

        list(items = itemLabels(object), transactionID = transactionID)
    })

