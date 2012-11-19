library("arules")

set.seed(20070611)

### Generate random data and coerce data to itemMatrix.
m <- matrix(as.integer(runif(50)>0.8), ncol=5)
dimnames(m) <- list(paste("trans", seq(nrow(m)), sep=""), 
	paste("item", seq(ncol(m)), sep=""))
i <- as(m, "itemMatrix")
i

### number of rows
length(i)

### subsetting
as(i[1:5], "matrix")
as(i[1:5,1:2], "matrix")
as(i[-1,-1], "matrix")
as(i[rep(c(T,F), nrow(m)/2), c(T,F,F,F,T)], "matrix")

### dimnames
dimnames(i)
dimnames(i[1:5,1:2])
dimnames(i[c("trans1", "trans10"), c("item5", "item1")])
dimnames(i[c("trans1", "trans2"), c("item5", "item4")])

if(!is(try(i[,c(1,1)], silent=TRUE), "try-error")) stop("Test for unique items failed!")


