library("arules")
data("Adult_transactions")
summary(Adult_transactions)


rules <- apriori(Adult_transactions[1:40000], 
	parameter = list(support = 0.05))
rules

error <- try(apriori(Adult_transactions, parameter = list(support = 1.3)))
error

rules.sub <- subset(rules, subset = rhs %in% "sex" & lift > 1.4)
summary(rules.sub)

as(rules.sub[1:3],"data.frame") 


