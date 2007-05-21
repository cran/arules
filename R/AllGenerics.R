
if(!isGeneric("%in%")) {
    setGeneric("%in%",
               function(x, table) standardGeneric("%in%"))
}

if(!isGeneric("%pin%")) {
    setGeneric("%pin%",
               function(x, table) standardGeneric("%pin%"))
}

if(!isGeneric("%ain%")) {
    setGeneric("%ain%",
               function(x, table) standardGeneric("%ain%"))
}

if(!isGeneric("SORT")) {
    setGeneric("SORT",
               function(x, ...) standardGeneric("SORT"))
}

if(!isGeneric("LIST")) {
    setGeneric("LIST",
               function(from, ...) standardGeneric("LIST"))
}

if(!isGeneric("WRITE")) {
  setGeneric("WRITE",
    function(x, ...) standardGeneric("WRITE"))
}

if(!isGeneric("coverage")) {
    setGeneric("coverage",
               function(x) standardGeneric("coverage"))
}

if(!isGeneric("crossTable")) {
    setGeneric("crossTable",
               function(x, ...) standardGeneric("crossTable"))
}

if(!isGeneric("decode")) {
    setGeneric("decode",
               function(x, ...) standardGeneric("decode"))
}

if(!isGeneric("duplicated")) {
    setGeneric("duplicated",
               function(x, incomparables = FALSE, ...) 
	       standardGeneric("duplicated"))
}

if(!isGeneric("encode")) {
    setGeneric("encode",
               function(x, ...) standardGeneric("encode"))
}

if(!isGeneric("generatingItemsets")) {
    setGeneric("generatingItemsets",
               function(x) standardGeneric("generatingItemsets"))
}

if(!isGeneric("inspect"))
    setGeneric("inspect",
               function(x, ...) standardGeneric("inspect"))

if(!isGeneric("intersect"))
    setGeneric("intersect",
               function(x, y) standardGeneric("intersect"))

if(!isGeneric("itemInfo")) {
    setGeneric("itemInfo",
               function(object) standardGeneric("itemInfo"))
}

if(!isGeneric("itemInfo<-")) {
    setGeneric("itemInfo<-",
               function(object, value) standardGeneric("itemInfo<-"))
}

if(!isGeneric("itemsetInfo")) {
    setGeneric("itemsetInfo",
               function(object) standardGeneric("itemsetInfo"))
}

if(!isGeneric("itemsetInfo<-")) {
    setGeneric("itemsetInfo<-",
               function(object, value) standardGeneric("itemsetInfo<-"))
}

if(!isGeneric("itemLabels")) {
    setGeneric("itemLabels",
               function(object, ...) standardGeneric("itemLabels"))
}

if(!isGeneric("itemLabels<-")) {
    setGeneric("itemLabels<-",
               function(object, value) standardGeneric("itemLabels<-"))
}

if(!isGeneric("items")) {
    setGeneric("items",
               function(x) standardGeneric("items"))
}

if(!isGeneric("items<-")) {
    setGeneric("items<-",
               function(x, value) standardGeneric("items<-"))
}

if(!isGeneric("itemFrequency")) {
    setGeneric("itemFrequency",
               function(x, ...) 
	       standardGeneric("itemFrequency"))
}

if(!isGeneric("itemFrequencyPlot")) {
    setGeneric("itemFrequencyPlot",
               function(x, ...) standardGeneric("itemFrequencyPlot"))
}

if(!isGeneric("is.element")) {
    setGeneric("is.element",
               function(el, set) standardGeneric("is.element"))
}

if(!isGeneric("is.superset")) {
    setGeneric("is.superset",
               function(x, y = NULL, proper = FALSE) 
	       standardGeneric("is.superset"))
}

if(!isGeneric("is.subset")) {
    setGeneric("is.subset",
               function(x, y = NULL, proper = FALSE) 
	       standardGeneric("is.subset"))
}

if(!isGeneric("labels")) {
    setGeneric("labels",
               function(object, ...) standardGeneric("labels"))
}

if(!isGeneric("lhs")) {
    setGeneric("lhs",
               function(x) standardGeneric("lhs"))
}

if(!isGeneric("lhs<-")) {
    setGeneric("lhs<-",
               function(x, value) standardGeneric("lhs<-"))
}


if(!isGeneric("match")) {
    setGeneric("match",
               function(x,  table, nomatch = NA, incomparables = FALSE) 
	       standardGeneric("match"))
}

if(!isGeneric("interestMeasure")) {
    setGeneric("interestMeasure",
               function(x,  method, transactions = NULL, ...) 
	       standardGeneric("interestMeasure"))
}


if(!isGeneric("nitems")) {
    setGeneric("nitems",
               function(x, ...) standardGeneric("nitems"))
}

if(!isGeneric("is.closed")) {
    setGeneric("is.closed",
               function(x) standardGeneric("is.closed"))
}

if(!isGeneric("is.maximal")) {
    setGeneric("is.maximal",
               function(x, ...) standardGeneric("is.maximal"))
}


if(!isGeneric("quality")) {
    setGeneric("quality",
               function(x) standardGeneric("quality"))
}

if(!isGeneric("quality<-")) {
    setGeneric("quality<-",
               function(x, value) standardGeneric("quality<-"))
}

if(!isGeneric("recode")) {
    setGeneric("recode",
               function(x, ...) standardGeneric("recode"))
}

if(!isGeneric("rhs")) {
    setGeneric("rhs",
               function(x) standardGeneric("rhs"))
}

if(!isGeneric("rhs<-")) {
    setGeneric("rhs<-",
               function(x, value) standardGeneric("rhs<-"))
}

if(!isGeneric("ruleInduction")) {
    setGeneric("ruleInduction",
               function(x, ...) standardGeneric("ruleInduction"))
}

if(!isGeneric("sample")) {
  setGeneric("sample",
      function(x, size, replace = FALSE, prob = NULL)
      standardGeneric("sample"))
}

if(!isGeneric("setdiff")) {
    setGeneric("setdiff",
               function(x, y) standardGeneric("setdiff"))
	   }

if(!isGeneric("setequal")) {
    setGeneric("setequal",
               function(x, y) standardGeneric("setequal"))
	   }
	     
if(!isGeneric("support")) {
    setGeneric("support",
               function(x, transactions, ...) 
	       standardGeneric("support"))
}

if(!isGeneric("size")) {
    setGeneric("size",
               function(x, ...) standardGeneric("size"))
}

if(!isGeneric("subset")) {
    setGeneric("subset",
               function(x, ...) standardGeneric("subset"))
}

if(!isGeneric("summary")) {
    setGeneric("summary",
               function(object, ...) standardGeneric("summary"))
}

if(!isGeneric("tidLists")) {
    setGeneric("tidLists", function(x) standardGeneric("tidLists"))
}

if(!isGeneric("transactionInfo")) {
    setGeneric("transactionInfo",
               function(x) standardGeneric("transactionInfo"))
}

if(!isGeneric("transactionInfo<-")) {
    setGeneric("transactionInfo<-",
               function(x, value) standardGeneric("transactionInfo<-"))
}

if(!isGeneric("union")) {
    setGeneric("union",
               function(x, y) standardGeneric("union"))
}

if(!isGeneric("unique")) {
    setGeneric("unique",
               function(x, incomparables = FALSE, ...) standardGeneric("unique"))
}

##***************************************************************
## for clustering

if(!isGeneric("affinity")) {
  setGeneric("affinity",
    function(x) standardGeneric("affinity"))
}


if(!isGeneric("dissimilarity")) {
  setGeneric("dissimilarity",
    function(x, y = NULL, method = NULL, args = NULL, ...)
    standardGeneric("dissimilarity"))
}

if(!isGeneric("predict")) {
    setGeneric("predict",
        function(object, ...) standardGeneric("predict"))
}


