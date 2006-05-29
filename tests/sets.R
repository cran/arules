library("arules")

data <- list(
    c("A", "B"),
    c("A", "B", "C", "G"),
    c("C", "D"),
    c("E", "F"),
    c("A", "B", "C", "D"),
    )


### is.superset / is.subset
is <- new("itemsets",  items = as(data, "itemMatrix"))

### find supersets in is
is.superset(is, is)
is.superset(is)
is.superset(is, is, proper = TRUE)

is.superset(is[5], is)


### find subsets in is
is.subset(is, is)
is.subset(is, is, proper = TRUE)
   
is.subset(is[1], is)

### is.maximal
quality(is) <- data.frame(isMaximal = is.maximal(is))

inspect(is)


### is.closed
db <- as(data, "transactions")
is <- eclat(db, parameter = list(supp = 0))

quality(is) <- cbind(quality(is),
    isClosed = is.closed(is))
  
inspect(is)




