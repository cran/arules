###*******************************************************
### Cross-tabulate joint purchases across pairs of items
###

setMethod("crossTable", signature(x = "itemMatrix"),
  function(x, ...) {
    crossprod(as(x, "matrix"))
  })
