library("arules")

m <- matrix(as.integer(runif(100000)>0.8), ncol=20)
dimnames(m) <- list(NULL,paste("item",c(1:20),sep=""))
t <- as(m, "transactions")
t

r <- apriori(t,parameter=list(supp=0.01,conf=0.1))
r
summary(r)
as(r,"data.frame")
as(subset(r, subset=lift>1.4 & lhs %in% "item3"),"data.frame")



f <- eclat(t,parameter=list(supp=0.01))
f
summary(f)
as(f,"data.frame")
inspect(subset(f, subset = items %in% "item7"))
