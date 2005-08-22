.onLoad <- function(lib, pkg) {
  require("methods", character = TRUE, quietly = TRUE)
}


###**********************************************************
### itemMatrix

setClass("itemMatrix",
    representation(data = "dgCMatrix", 
      itemInfo = "data.frame"), 
    validity = function(object) {

### regular itemMatrix
    if(length(itemInfo(object)[["labels"]]) == dim(object)[2]) return(TRUE)
    return("number of item labels does not match number of columns in itemMatrix")

    })


setClass("summary.itemMatrix",
    representation(Dim = "integer",
      itemSummary = "integer",
      lengths = "table",
      lengthSummary = "table",
      itemInfo = "data.frame"))


###**********************************************************
### transactions 

setClass("transactions",
    representation(transactionInfo = "data.frame"),
    contains = c("itemMatrix"),
    prototype(transactionInfo = data.frame(),
      labels = data.frame()),
    validity = function(object) {
### check dimensions
### no transactionInfo (empty data.frame)
    if(length(object@transactionInfo) == 0) return(TRUE) 


    if (length(object) == length(object@transactionInfo[[1]]))
    return(TRUE)

    return("length of transactionInfo does not match number of transactions")
    })

setClass("summary.transactions",
    representation(transactionInfo = "data.frame"),
    contains = c("summary.itemMatrix")
    )


###**********************************************************
### transaction ID lists

setClass("tidLists",
    representation(data = "dgCMatrix",
       itemInfo = "data.frame",
       transactionInfo = "data.frame"),
    prototype(transactionInfo = data.frame(),
      itemInfo = data.frame()
      ),
    validity = function(object) { 
### check number of labels in itemInfo
### no labels (empty data.frame)
    if(length(object@itemInfo) == 0) return(TRUE) 

    if(length(itemInfo(object)[["labels"]]) == dim(object)[1]) return(TRUE)
    else return("number of item/itemset labels does not match number of rows in tidLists")

### fixme: length of transactionInfo missing
    })

setClassUnion("tidLists_or_NULL", c("tidLists", "NULL"))

setClass("summary.tidLists",
    representation(Dim = "integer")
    )


###**********************************************************
### mining parameter

setClass("APappearance",
    representation(set = "integer",
      items = "integer",
      labels = "character",
      default = "character"),
    prototype(set = as.integer(rep(0, 5)),
      items = integer(),
      labels = "",
      default = "both"),
    validity = function(object) {
    if (!object@default %in% c("lhs", "rhs", "none", "both")) 
    return("Default value not specified correctly")
    else if (!sum(object@set) == length(object@items)) 
    return("Slots 'set' and 'items' do not match")
    return(TRUE)
    })


setClass("ASparameter",
    representation(support = "numeric",
      minlen = "integer",
      maxlen = "integer",
      target = "character",
      ext = "logical"),
    prototype(target = "frequent itemsets",
      support = 0.1,
      minlen = as.integer(1),
      maxlen = as.integer(5),
      ext = FALSE),
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
    representation(confidence = "numeric",
      minval = "numeric",
      smax = "numeric",
      arem = "character",
      aval = "logical",
      originalSupport = "logical"),
    contains = "ASparameter",
    prototype(new("ASparameter"),
      target = "rules",
      confidence = 0.8,
      minval = 0.1,
      smax = 1.0,
      arem = "none",
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
    representation(sort = "integer",
      verbose = "logical"),
    prototype(verbose = TRUE,
      sort = as.integer(2)),
    validity = function(object) {
    if (object@sort > 2 | object@sort < -2) 
    return(paste("sort =", object@sort,"not one of 1: ascending,",
	"-1: descending, 0: do not sort, 2: ascending,",
	"-2: descending w.r.t. transaction size sum"))
    else return(TRUE) 
    })

setClass("APcontrol",
    representation(filter = "numeric",
      tree = "logical",
      heap = "logical",
      memopt = "logical",
      load = "logical"),
    contains = "AScontrol",
    prototype(new("AScontrol"),
      filter = 0.1,
      sort = as.integer(2),
      tree = TRUE,
      heap = TRUE,
      memopt = FALSE,
      load = TRUE),
    validity = function(object) {
    if (object@filter > 1 || object@filter < -1) 
    return(paste("filter =", object@filter, "is not in [-1,1]"))
    else return(TRUE)
    })

setClass("ECcontrol",
    representation(sparse = "numeric"),
    contains = "AScontrol",
    prototype(new("AScontrol"),
      sparse = 7,
      sort = as.integer(-2)))



###****************************************************************
### associations

  setClass("associations",
      representation(quality = "data.frame",
	"VIRTUAL"))

  setClass("itemsets",
      representation(items = "itemMatrix", tidLists = "tidLists_or_NULL"),
      contains = "associations",
      prototype(tidLists = NULL),
      validity = function(object) {
### if tidLists exists, check dimensions
### Note, we cannot check dim(object@tidLists)[2] here since we
### don't know the number of transactions in the used data set! 
      if (!is.null(object@tidLists) && 
	length(object@tidLists) != length(object@items))
      return("mismatch between number of itemsets and length of tidLists")

### if quality exists, check dimensions
      if (!length(object@quality) && 
	dim(object@quality)[1] != length(object@items))
      return("mismatch between number of items and rows in quality")

### check items
      i <- validObject(object@items, test=TRUE)
      if(i != TRUE) return(paste("problem with items:",i))

      return(TRUE)
      })



### rules is the lhs
setClass("rules",
    representation(lhs = "itemMatrix", rhs = "itemMatrix"),
    contains = "associations",
    validity = function(object) {

### check dimensions
    if (!all(dim(object@lhs) == dim(object@rhs))) 
    return("dimension mismatch between lhs and rhs")
    if (!length(object@quality) && 
      dim(object@quality)[1] != length(object@lhs))
    return("mismatch between number of rules and rows in quality")

### check lhs, rhs
    i <- validObject(object@lhs, test=TRUE)
    if(i != TRUE) return(paste("problem with lhs:",i))
    i <- validObject(object@rhs, test=TRUE)
    if(i != TRUE) return(paste("problem with rhs:",i))

    return(TRUE)
    })

setClass("summary.associations",
    representation(length ="integer", quality = "table",
      "VIRTUAL"))

setClass("summary.itemsets",
    representation(tidLists = "logical", items = "summary.itemMatrix"),
    contains = "summary.associations")

setClass("summary.rules",
    representation(lengths = "table",
      lengthSummary = "table"),
    contains = "summary.associations")

