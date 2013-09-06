library("arules")

options(digits=2)

data <- list(
    c("a","b","c"),
    c("a","b"),
    c("a","b","d"),
    c("b","e"),
    c("a","c"),
    c("c","e"),
    c("a","b","d","e")
    )
names(data) <- paste("Tr",c(1:7), sep = "")
t <- as(data, "transactions")


##########################################################
# test the apriori interface

### rules
r <- apriori(t, parameter=list(supp=0.25, conf=0), control=list(verb=FALSE))
r
summary(r)
inspect(sort(r, by = "lift")[1:7])

### test appearance
r <- apriori(t, parameter=list(supp=0.25, conf=0),
     appearance=list(rhs=c("a","b"), default= "lhs"), control=list(verb=FALSE))
inspect(r)

### test lhs.support
r <- apriori(t, parameter=list(supp=0.25, conf=0, originalSupp=FALSE, ext=TRUE),
    control=list(verb=FALSE))
inspect(r[1:2])




##########################################################
# test the eclat interface

f <- eclat(t, control=list(verb=FALSE))
f
summary(f)
inspect(f[1:2])
labels(f[1:2])


### test subset
f.sub <- subset(f, subset=items %in% "a")
labels(f.sub)

### test tidlists
f <- eclat(t, parameter = list(tidLists = TRUE), control=list(verb=FALSE))
f
summary(f)
tl <- tidLists(f)
tl
summary(tl)

as(tl[1:5], "list")
