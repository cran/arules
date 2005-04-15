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

f <- eclat(t)
f
summary(f)
inspect(f[1:2])
labels(f[1:2])


### test subset
f.sub <- subset(f, subset=items %in% "a")
labels(f.sub)

### test tidlist
f <- eclat(t, parameter = list(tidList = TRUE))
f
summary(f)
tl <- tidList(f)
tl
summary(tl)

as(tl[1:5], "list")

