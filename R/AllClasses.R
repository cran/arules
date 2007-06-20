.onLoad <- function(lib, pkg) {
    require("methods", character = TRUE, quietly = TRUE)
    ## 
    require("Matrix", character = TRUE, quietly = TRUE)
    cat("** fixing ngCMatrix validation\n")
    setValidity("ngCMatrix",
        function(object) .Call("R_valid_ngCMatrix", object),
                 where = .GlobalEnv)
}


##**********************************************************
## itemMatrix

setClass("itemMatrix",
    representation(
        data        = "ngCMatrix", 
        itemInfo    = "data.frame",
        itemsetInfo = "data.frame"
    ), 
    
    prototype(itemInfo = data.frame(), itemsetInfo = data.frame()),
    
    validity= function(object) {
        ## itemInfo needs a labels column of appropriate length
        ## also the labels must be unique for matching objects.
        if (length(object@itemInfo$labels) != nitems(object))
            return("item labels do not match number of columns")
        if (length(unique(object@itemInfo$labels)) != nitems(object))
            return("item labels not unique")

        if (length(object@itemsetInfo) && 
            length(object@itemsetInfo[[1]]) != length(object))
            return("itemsetInfo does not match number of rows")

        TRUE
    }
)

setClass("summary.itemMatrix",
    representation(
        Dim           = "integer",
        itemSummary   = "integer",
        lengths       = "table",
        lengthSummary = "table",
        itemInfo      = "data.frame"
    )
)

##**********************************************************
## transactions 

setClass("transactions",
    representation(
        transactionInfo = "data.frame"
    ),
    contains = "itemMatrix",
    
    prototype(transactionInfo = data.frame()),
    
    validity = function(object) {
        ## check dimensions
        ## no transactionInfo (empty data.frame)
        if (length(object@transactionInfo) &&
            length(object@transactionInfo[[1]]) != length(object))
           return("transactionInfo does not match number of transactions")

        TRUE
    }
)

setClass("summary.transactions",
    representation(transactionInfo = "data.frame"),
    contains = "summary.itemMatrix"
)


##**********************************************************
## transaction ID lists
##
## FIXME optional item labels is not a good idea!

setClass("tidLists",
    representation(
        data            = "ngCMatrix",
        itemInfo        = "data.frame",
        transactionInfo = "data.frame"
    ),
    
    prototype(itemInfo = data.frame(), transactionInfo = data.frame()),
    
    validity = function(object) { 
        ## check number of labels in itemInfo
        ## no labels (empty data.frame)
        if (length(object@itemInfo) &&
            length(object@itemInfo[["labels"]]) != dim(object)[1])
            return("number of item labels does not match number of rows")

        if (length(object@transactionInfo) &&
            length(object@transactionInfo[[1]]) != dim(object)[2])
            return("transactionInfo does not match number of transactions")

        TRUE
    }
)

setClassUnion("tidLists_or_NULL", c("tidLists", "NULL"))

setClass("summary.tidLists",
    representation(
        Dim                = "integer",
        transactionSummary = "integer",
        lengths            = "table",
        lengthSummary      = "table",
        itemInfo           = "data.frame"
    )
)


##**********************************************************
## mining parameter

setClass("APappearance",
    representation(
        set     = "integer",
        items   = "integer",
        labels  = "character",
        default = "character"),

    prototype(
        set     = rep(0L, 5),
        items   = integer(),
        labels  = "",
        default = "both"),
    
    validity = function(object) {
        if (!object@default %in% c("lhs", "rhs", "none", "both")) 
        return("Default value not specified correctly")
        else if (!sum(object@set) == length(object@items)) 
        return("Slots 'set' and 'items' do not match")
        return(TRUE)
    })


setClass("ASparameter",
    representation(
        support = "numeric",
        minlen  = "integer",
        maxlen  = "integer",
        target  = "character",
        ext     = "logical"),
    
    prototype(
        target  = "frequent itemsets",
        support = 0.1,
        minlen  = 1L,
        maxlen  = 5L,
        ext     = FALSE),
    
    validity = function(object) {
        if (!object@target %in% .types()) 
        return(paste("target =", object@target, "not supported."))
        if (object@support > 1) 
        return(paste("support =", object@support, "> 1"))
        if (object@minlen <= 0)  
        return(paste("minlen =", object@minlen, "<= 0"))
        if (object@minlen > object@maxlen) 
        return(paste("minlen =", object@minlen,
                "> maxlen =", object@maxlen))
        return(TRUE)
    })

setClass("APparameter",
    representation(
        confidence  = "numeric",
        minval      = "numeric",
        smax        = "numeric",
        arem        = "character",
        aval        = "logical",
        originalSupport = "logical"),
    contains = "ASparameter",
    
    prototype(new("ASparameter"),
        target      = "rules",
        confidence  = 0.8,
        minval      = 0.1,
        smax        = 1.0,
        arem        = "none",
        originalSupport = TRUE,
        aval = FALSE),
    
    validity = function(object) {
        if (!object@arem %in% .aremtypes()) 
        return(paste("arem =", object@arem, "not supported."))
        if (object@confidence > 1)  
        return(paste("confidence =", object@confidene, "> 1"))
        if (object@smax < 0) 
        return(paste("smax =", object@smax, "< 0"))
        return(TRUE)
    })

setClass("ECparameter",
    representation(tidLists = "logical"),
    contains = "ASparameter",
    
    prototype(new("ASparameter"),
        tidLists = FALSE),
    
    validity = function(object) {
        if (object@target %in% .types(method = "eclat")) return(TRUE) 
        else return(paste(object@target, "not supported"))
    })

setClass("AScontrol",
    representation(
        sort    = "integer",
        verbose = "logical"),
    
    prototype(
        verbose = TRUE,
        sort    = 2L),
    
    validity = function(object) {
        if (object@sort > 2 | object@sort < -2) 
        return(paste("sort =", object@sort,"not one of 1: ascending,",
                "-1: descending, 0: do not sort, 2: ascending,",
                "-2: descending w.r.t. transaction size sum"))
        else return(TRUE) 
    })

setClass("APcontrol",
    representation(
        filter  = "numeric",
        tree    = "logical",
        heap    = "logical",
        memopt  = "logical",
        load    = "logical"),
    contains    = "AScontrol",
    
    prototype(new("AScontrol"),
        filter  = 0.1,
        sort    = 2L,
        tree    = TRUE,
        heap    = TRUE,
        memopt  = FALSE,
        load    = TRUE),
    
    validity = function(object) {
        if (object@filter > 1 || object@filter < -1) 
        return(paste("filter =", object@filter, "is not in [-1,1]"))
        else return(TRUE)
    })

setClass("ECcontrol",
    representation(sparse = "numeric"),
    contains = "AScontrol",
    
    prototype(new("AScontrol"),
        sparse  = 7,
        sort    = -2L))



##****************************************************************
## associations

setClass("associations",
    representation(quality = "data.frame", "VIRTUAL"))

setClass("itemsets",
    representation(
        items    = "itemMatrix", 
        tidLists = "tidLists_or_NULL"
    ),

    contains = "associations",
    
    prototype(tidLists = NULL, quality = data.frame()),
    
    validity = function(object) {
        ## if tidLists exists, check dimensions
        ## Note, we cannot check dim(object@tidLists)[2] here since we
        ## don't know the number of transactions in the used data set! 
        if (length(object@tidLists) && 
            length(object@tidLists) != length(object@items))
            return("tidLists does not match number of itemsets")

        ## if quality exists, check dimensions
        if (length(object@quality) && 
            length(object@quality[[1]]) != length(object@items))
            return("quality does not match number of itemsets")

        TRUE
    }
)

## rules is the lhs
setClass("rules",
    representation(
        lhs = "itemMatrix", 
        rhs = "itemMatrix"
    ),
    contains = "associations",
    
    prototype(quality = data.frame()),
    
    validity = function(object) {
        ## check dimensions
        if (!all(dim(object@lhs) == dim(object@rhs))) 
            return("dimensions of lhs and rhs do not match")

        ## check quality
        if (length(object@quality) && 
            length(object@quality[[1]]) != length(object@lhs))
            return("quality does not match the number of rules")

        TRUE
    }
)

setClass("summary.associations",
    representation(length  ="integer", quality = "table", "VIRTUAL"))

setClass("summary.itemsets",
    representation(tidLists = "logical", items = "summary.itemMatrix"),
    contains = "summary.associations")

setClass("summary.rules",
    representation(lengths = "table", lengthSummary = "table"),
    contains = "summary.associations")


##**********************************************************
## proximities for clustering

## we reuse S3 dist for compatibility reasons
setOldClass("dist")

## similarity
setClass("ar_similarity",
    contains = "matrix",
    representation(method = "character")
)

## distances between x and y
setClass("ar_cross_dissimilarity",
    contains = "matrix",
    representation(method = "character")
)

###
