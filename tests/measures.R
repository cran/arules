library("arules")

options(digits=2)

data <- list(
    c("A", "B"),
    c("A", "B", "C", "G"),
    c("C", "D"),
    c("C", "D"),
    c("E", "F")
    )

trans <- as(data, "transactions")


##################################################################
# Test the original example from
# Edward R. Omiecinski. Alternative interest measures for mining 
# associations in databases. IEEE Transactions on Knowledge and 
# Data Engineering, 15(1):57-69, Jan/Feb 2003.

fsets <- eclat(trans, parameter = list(supp = 0), control=list(verb=FALSE))

quality(fsets) <- cbind(quality(fsets), 
  allConfidence = interestMeasure(fsets, method = "allConfidence", trans))

inspect(fsets[order(size(fsets))])

###################################################################
## test all measures for itemsets

measures <- c("support", "allConfidence", "crossSupportRatio")

m1 <- interestMeasure(fsets, measures, trans)
m1

## now dont reuse any quality data
m2 <- interestMeasure(fsets, measures, trans, reuse = FALSE)
m2

## compare results
all.equal(m1, m2)


###################################################################
# test all measures for rules

rules <- apriori(trans, parameter=list(supp=0.01, conf = 0.5), 
        control=list(verb=FALSE))
 
## calculate all measures
measures <-  c("support", "coverage", "confidence", "lift",
    "leverage", "hyperLift", "hyperConfidence", "improvement",
    "chiSquare", "cosine", "conviction", "gini", "oddsRatio", "phi", "doc")

m1 <- interestMeasure(rules, measures, trans)
m1

## now dont reuse any quality data
m2 <- interestMeasure(rules, measures, trans, reuse = FALSE)
m2

## compare results
all.equal(m1, m2)
