library("testthat")
library("arules")

context("associations")

set.seed(20070611)

m <- matrix(as.integer(runif(100000)>0.8), ncol=20)
dimnames(m) <- list(NULL,paste("item",c(1:20),sep=""))
t <- as(m, "transactions")
#t
#inspect(t[10])

expect_identical(dim(t), dim(m))


r <- apriori(t,parameter=list(supp=0.01,conf=0.1),control=list(verb=FALSE))
#r
#summary(r)
#inspect(r)

ss <- subset(r, subset=lift>1.4 & lhs %in% "item3")
#inspect(ss)
expect_identical(labels(lhs(ss)), "{item2,item3}")
expect_true(quality(ss)$lift>1.4)


f <- eclat(t,parameter=list(supp=0.01),control=list(verb=FALSE))
#f
#summary(f)
#inspect(f)

ss <- subset(f, subset = items %in% "item7")
#inspect(ss)
expect_identical(labels(ss), grep("item7", labels(ss), value = TRUE))


### create associations manually
lmat <- matrix(rbind(c(1,1,0), c(0,0,1)), ncol=3)
rmat <- matrix(rbind(c(1,0,0), c(0,1,0)), ncol=3)
colnames(lmat) <- c("a", "b", "c")
colnames(rmat) <- c("c", "a", "b")
# Note: the column names do not agree!

lhs <- as(lmat, "itemMatrix")
rhs <- as(rmat, "itemMatrix")

is <- new("itemsets", items=lhs, quality=data.frame(support=c(.1,.1)))
#inspect(is)

expect_equal(labels(is), c("{a,b}", "{c}"))

qual <- data.frame(support=c(.5,.5), confidence=c(.5,.5), lift=c(2,1))

## warning because of the disagreeing labels
expect_warning(
  r <- new("rules", lhs=lhs, rhs=rhs, quality=qual)
)

#inspect(r)
