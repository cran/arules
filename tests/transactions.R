library("arules")

data <- list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("b","e"),
  c("a","d"),
  c("d","e"),
  c("a","b","d","e","f","g"),
    )
names(data) <- paste("Tr",c(1:7), sep = "")




##################################################
### test transactions

t <- as(data, "transactions")
t
summary(t)
inspect(t[1:2])

m <- as(t, "matrix")
m
as(as(m, "transactions"), "list")

as(t, "list")
LIST(t, decode = FALSE)





##########################################################################
### compare transactions with missing "c", "f", "g"

t <- as(data, "transactions")[2:6]
t_comp <- as(data[2:6], "transactions")

as(t, "dgCMatrix")
as(t_comp, "dgCMatrix")


### test apriori and eclat
fsets <- apriori(t, parameter = list(target = "frequ", supp = 0.2))
esets <- eclat(t, parameter = list(target = "frequ", supp = 0.2, tidList = TRUE))
fsets_comp <- apriori(t_comp, parameter = list(target = "frequ", supp = 0.2))
esets_comp <- eclat(t_comp, parameter = list(target = "frequ", supp = 0.2))

as(items(fsets), "dgCMatrix")
as(items(fsets_comp), "dgCMatrix")

as(items(esets), "dgCMatrix")
as(items(esets_comp), "dgCMatrix")




##########################################################################
### test tidLists

tl <- (as(t,"tidLists"))
tl
as(tl, "list")
as(tl, "matrix")
inspect(as(tl, "transactions"))


