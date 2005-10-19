###*******************************************************
### Function sample
###
### sample from transactions or associations

setMethod("sample", signature(x = "itemMatrix"),
    function(x, size, replace = FALSE, prob = NULL) {
    index <- sample(c(1:length(x)), size = size, replace = replace, prob = prob)
    x[index]
})

setMethod("sample", signature(x = "associations"),
    function(x, size, replace = FALSE, prob = NULL) {
    index <- sample(c(1:length(x)), size = size, replace = replace, prob = prob)
    x[index]
})

