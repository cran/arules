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

### transactions
t <- as(data, "transactions")
t
summary(t)
inspect(t[1:2])

m <- as(t, "matrix")
m
as(as(m, "transactions"), "list")

as(t, "list")
LIST(t, decode = FALSE)


