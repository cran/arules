library("arules")

# Test the original example from
# Edward R. Omiecinski. Alternative interest measures for mining 
# associations in databases. IEEE Transactions on Knowledge and 
# Data Engineering, 15(1):57-69, Jan/Feb 2003.


data <- list(
    c("A", "B"),
    c("A", "B", "C", "G"),
    c("C", "D"),
    c("C", "D"),
    c("E", "F"),
    )

trans <- as(data, "transactions")
fsets <- eclat(trans, parameter = list(supp = 0))

quality(fsets) <- cbind(quality(fsets), allConfidence(fsets))

inspect(fsets[order(size(fsets))])

  
  
