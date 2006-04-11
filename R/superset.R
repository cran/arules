###*****************************************************************
### arules specific set methods: is.superset, is.subset (only for itemMatrix)


setMethod("is.superset", signature(set = "itemMatrix", el = "itemMatrix"),
  function(el, set, proper = FALSE) {
    isss <- function(el.ind, set, proper = FALSE) {
      ## select item columns
      set.sub <- set[,el.ind]
      ## find sets where all items in el are present
      res <- size(set.sub) == length(el.ind)
      if(proper) res <- res & size(set) > length(el.ind)
      t(res)
    }

    ## find indices for item columns in el
    el.ind <- LIST(el, decode = FALSE)
    res <- sapply(el.ind, isss, set = set, proper = proper)
    #dimnames(res) <- list(labels(set), labels(el))
    res
  })

setMethod("is.superset", signature(set = "associations", el = "associations"),
  function(el, set, proper = FALSE) {
    is.superset(items(set), items(el), proper)
  })


### subset

setMethod("is.subset", signature(set = "itemMatrix", el = "itemMatrix"),
  function(set, el, proper = FALSE) {
    is.superset(set, el, proper = proper)
  })

setMethod("is.subset", signature(set = "associations", el = "associations"),
  function(set, el, proper = FALSE) {
    is.subset(items(set), items(el), proper)
  })



