###*******************************************************
### Function support
###
### return  support of itemsets in transactions usind tid-list intersections

setMethod("support", signature(x = "itemMatrix"),
    function(x, transactions, type= c("relative", "absolute")) {
      type <- match.arg(type)
     
      if (dim(x)[2] != dim(transactions)[2])
             stop("number of items in x and transactions do not match.")
	     
      ### prepare tid-list and list of itemsets
      tlists <- LIST(as(transactions, "tidLists"), decode = FALSE)
      xitems <- LIST(x, decode = FALSE)
      
      ### select tid-lists for items and do intersection
      supports <- sapply(xitems, FUN = function(i) { 
      	cnt <- tabulate(unlist(tlists[i])) 
	sum(cnt == length(i))
	})

     


      names(supports) <- labels(x)$elements

      switch(type,
	relative =  supports/length(transactions),
	absolute =  supports)
      })


### wrapper method for associations
setMethod("support", signature(x = "associations"),
    function(x, transactions, type= c("relative", "absolute")) {
    support(items(x), transactions = transactions, type = type)
    }
    )

