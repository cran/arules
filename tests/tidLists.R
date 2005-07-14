library("arules")

data <- list(
    c("a","b","c"),
    c("a","b"),
    c("a","b","d"),
    c("b","e"),
    c("a","c"),
    c("c","e"),
    c("a","b","d","e"),
    )
names(data) <- paste("Tr",c(1:7), sep = "")

t <- as(data, "transactions")

### test tidLists
tl <- (as(t,"tidLists"))
tl
as(tl, "list")
as(tl, "matrix")
inspect(as(tl, "transactions"))

