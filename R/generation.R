###*******************************************************
### Function random.transactions
###
### Generate a random transaction data set. Each item 
###   has the same success probability (prob) to be part 
###   of a transactions.

random.transactions <- function(nitems, ntrans, prob = 0.1) {

  # check prob
  if(any(prob>1 | prob<0)) stop("Illegal probability given (>1 or <0)!")
  
  # case if only one value for all items is given
  if(length(prob) == 1) prob <- rep(prob, nitems)

  # check length of prob
  if(length(prob) != nitems) 
    stop("Number of items and number of given probabilities do not match!")



  # simulate data (create a list with indices)
  simList <- replicate(ntrans,
              which(runif(nitems) <= prob))
          #   which(rbinom(nitems, 1, prob) == 1))
          ### the first version is slightly faster

  # create sparse matrix (transposed)
  i <- as.integer(unlist(simList) - 1)
  p <- as.integer(c(0, cumsum(sapply(simList, length))))
  x <- rep.int(1, length(i))

  data <- new("dgCMatrix", i = i, p = p, x = x,
      Dim = as.integer(c(nitems , ntrans)))

  new("transactions", data = data,
      itemInfo = data.frame(labels = paste("item", c(1:nitems), sep="" )),
      transactionInfo = data.frame(transactionID = 
        paste("trans", c(1:ntrans), sep="" ))
     )
}

