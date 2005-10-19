
if(!isGeneric("%in%")) {
    setGeneric("%in%",
               function(x, table) standardGeneric("%in%"))
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


if(!isGeneric("allConfidence")) {
    setGeneric("allConfidence",
               function(x, ...) standardGeneric("allConfidence"))
}

if(!isGeneric("decode")) {
    setGeneric("decode",
               function(x, code, ...) standardGeneric("decode"))
}

if(!isGeneric("duplicated")) {
    setGeneric("duplicated",
               function(x, incomparables = FALSE, ...) 
	       standardGeneric("duplicated"))
}

if(!isGeneric("encode")) {
    setGeneric("encode",
               function(x, code, ...) standardGeneric("encode"))
}

if(!isGeneric("hyperLift")) {
  setGeneric("hyperLift",
	     function(x, ...) standardGeneric("hyperLift"))
}

if(!isGeneric("hyperConfidence")) {
  setGeneric("hyperConfidence",
	     function(x, ...) standardGeneric("hyperConfidence"))
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
               function(x, code, ...) standardGeneric("recode"))
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

if(!isGeneric("setequal"))
    setGeneric("setequal",
               function(x, y) standardGeneric("setequal"))

if(!isGeneric("support")) {
    setGeneric("support",
               function(x, transactions, ...) 
	       standardGeneric("support"))
}

if(!isGeneric("size")) {
    setGeneric("size",
               function(x) standardGeneric("size"))
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

