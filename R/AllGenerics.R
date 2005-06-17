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

if(!isGeneric("all_confidence")) {
    setGeneric("all_confidence",
               function(x, ...) standardGeneric("all_confidence"))
}

if(!isGeneric("decode")) {
    setGeneric("decode",
               function(x, ...) standardGeneric("decode"))
}

if(!isGeneric("hyperlift")) {
    setGeneric("hyperlift",
               function(x, ...) standardGeneric("hyperlift"))
}

if(!isGeneric("inspect"))
    setGeneric("inspect",
               function(x, ...) standardGeneric("inspect"))

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

if(!isGeneric("itemSupport")) {
    setGeneric("itemSupport",
               function(x, ...) standardGeneric("itemSupport"))
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

if(!isGeneric("quality")) {
    setGeneric("quality",
               function(x) standardGeneric("quality"))
}

if(!isGeneric("quality<-")) {
    setGeneric("quality<-",
               function(x, value) standardGeneric("quality<-"))
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

if(!isGeneric("tidList")) {
    setGeneric("tidList", function(x) standardGeneric("tidList"))
}

if(!isGeneric("transactionInfo")) {
    setGeneric("transactionInfo",
               function(x) standardGeneric("transactionInfo"))
}

if(!isGeneric("transactionInfo<-")) {
    setGeneric("transactionInfo<-",
               function(x, value) standardGeneric("transactionInfo<-"))
}
