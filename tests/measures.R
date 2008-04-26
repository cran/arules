library("arules")


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
  allConfidence = interestMeasure(fsets, method = "allConfidence"))

inspect(fsets[order(size(fsets))])


###################################################################
# Test improvment
# R. Bayardo, R. Agrawal, and D. Gunopulos. Constraint-based rule mining in
# large, dense databases. Data Mining and Knowledge Discovery, 4(2/3):217-240,
# 2000

rules <- apriori(trans, parameter=list(supp=0.01, conf = 0.5), 
        control=list(verb=FALSE))
 
quality(rules) <- cbind(quality(rules),
  improvement = interestMeasure(rules, method = "improvement"))

inspect(rules)
    
