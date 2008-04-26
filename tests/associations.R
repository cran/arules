library("arules")

set.seed(20070611)

m <- matrix(as.integer(runif(100000)>0.8), ncol=20)
dimnames(m) <- list(NULL,paste("item",c(1:20),sep=""))
t <- as(m, "transactions")
t
inspect(t[10])

r <- apriori(t,parameter=list(supp=0.01,conf=0.1),control=list(verb=FALSE))
r
summary(r)
as(r,"data.frame")
as(subset(r, subset=lift>1.4 & lhs %in% "item3"),"data.frame")



f <- eclat(t,parameter=list(supp=0.01),control=list(verb=FALSE))
f
summary(f)
as(f,"data.frame")
inspect(subset(f, subset = items %in% "item7"))
