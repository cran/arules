##*******************************************************
## Function support
##
## return  support of itemsets in transactions using tid-list intersections

setMethod("support", signature(x = "itemMatrix", 
        transactions = "transactions"), 
    function(x, transactions, type= c("relative", "absolute"), control = NULL) {
        type <- match.arg(type)
        
        verbose <- if(is.null(control$v))   FALSE      else control$v
        method  <- if(is.null(control$m))   "tidlists" else control$m
        
        methods <- c("ptree", "tidlists")

        method <-  methods[pmatch(method , methods)]
        if(is.na(method)) error("unknown method")

        if(verbose) cat("using method:", method, "\n")

        tm <- system.time(
            supports <- 
            if(method == "ptree") support.ptree(x, transactions, control)
            else support.tidlists(x, transactions, control)
        )

        if(verbose) cat("timing:", tm[1]+ tm[2], "sec.\n")

        switch(type,
            relative =  supports/length(transactions),
            absolute =  supports
        )

    })


## we have now a C implementation
support.tidlists.inR <- function(x, transactions, control = NULL) {

        if (nitems(x) != nitems(transactions))
        stop("number of items in x and transactions do not match.")

        ## prepare tid-list and list of itemsets
        tlists <- LIST(as(transactions, "tidLists"), decode = FALSE)
        xitems <- LIST(x, decode = FALSE)

        ## select tid-lists for items and do intersection
        supports <- sapply(xitems, FUN = function(i) { 
                tidls <- unlist(tlists[i])
                if(!is.null(tidls)) 
                    supp <- sum(tabulate(tidls) == length(i))
                else supp <- 0 
                supp
            })

        #names(supports) <- labels(x)$elements
        supports
    }

support.tidlists <- function(x, transactions, control = NULL) {

        if (nitems(x) != nitems(transactions))
        stop("number of items in x and transactions do not match.")

        reduce  <- if(is.null(control$r))    FALSE else control$r
        if(reduce == TRUE) warning("method tidlists does not use reduce")

        tid <- as(transactions, "tidLists")

        support <- .Call("R_tid_support" ,tid@data, x@data)
        
        #names(supports) <- labels(x)$elements
        support
    }

support.ptree <- function(x, transactions, control = NULL) {
        reduce  <- if(is.null(control$r))    FALSE else control$r
        verbose <- if(is.null(control$v))    FALSE else control$v

        .Call("R_pncount", x@data, transactions@data, TRUE, reduce, verbose)
    }

## wrapper method for associations
setMethod("support", signature(x = "associations", 
        transactions = "transactions"),
    function(x, transactions, type= c("relative", "absolute"), control = NULL) 
    support(items(x), transactions = transactions, type = type, 
        control = control)
)

