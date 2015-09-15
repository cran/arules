library("arules")
library("testthat")

context("measures")

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

# complains about low support
expect_warning(
  fsets <- eclat(trans, parameter = list(supp = 0), control=list(verb=FALSE))
  )

# add all-confidence
quality(fsets)$allConfidence <- 
  interestMeasure(fsets, method = "allConfidence", trans)
#inspect(fsets[order(size(fsets))])

# check
ac <- c(1.00, 0.67, 0.33, 0.33, 0.50, 0.33, 0.33, 0.50, 0.50, 0.33, 0.33, 
  1.00, 0.33, 0.60, 0.40, 0.40, 0.20, 0.40, 0.20, 0.20)
expect_equal(round(quality(fsets)$allConfidence, 2), ac)

###################################################################
## test all measures for itemsets
measures <- c("support", "allConfidence", "crossSupportRatio")
m1 <- interestMeasure(fsets, measures, trans)

## now recalculate the measures using the transactios
m2 <- interestMeasure(fsets, measures, trans, reuse = FALSE)
expect_equal(m1, m2)


###################################################################
# test all measures for rules

expect_warning(
  rules <- apriori(trans, parameter=list(supp=0.01, conf = 0.5), 
    control=list(verb=FALSE))
)

## calculate all measures
measures <-  c("support", "coverage", "confidence", "lift",
    "leverage", "hyperLift", "hyperConfidence", "improvement",
    "chiSquare", "cosine", "conviction", "gini", "oddsRatio", "phi", "doc")

m1 <- interestMeasure(rules, measures, trans)
m2 <- interestMeasure(rules, measures, trans, reuse = FALSE)
expect_equal(m1, m2)

