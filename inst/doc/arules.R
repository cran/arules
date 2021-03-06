### R code from vignette source 'arules.Rnw'

###################################################
### code chunk number 1: arules.Rnw:73-76
###################################################
options(width = 75)
### for sampling
set.seed <- 1234


###################################################
### code chunk number 2: arules.Rnw:1172-1173
###################################################
library("arules")


###################################################
### code chunk number 3: epub1
###################################################
data("Epub")
Epub


###################################################
### code chunk number 4: epub2
###################################################
summary(Epub)


###################################################
### code chunk number 5: arules.Rnw:1198-1200
###################################################
year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y")
table(year)


###################################################
### code chunk number 6: arules.Rnw:1208-1211
###################################################
Epub2003 <- Epub[year == "2003"]
length(Epub2003)
image(Epub2003)


###################################################
### code chunk number 7: epub
###################################################
print(image(Epub2003))


###################################################
### code chunk number 8: arules.Rnw:1237-1238
###################################################
transactionInfo(Epub2003[size(Epub2003) > 20])


###################################################
### code chunk number 9: arules.Rnw:1250-1251
###################################################
inspect(Epub2003[1:5])


###################################################
### code chunk number 10: arules.Rnw:1257-1258
###################################################
as(Epub2003[1:5], "list")


###################################################
### code chunk number 11: arules.Rnw:1264-1266
###################################################
EpubTidLists <- as(Epub, "tidLists")
EpubTidLists


###################################################
### code chunk number 12: arules.Rnw:1273-1274
###################################################
as(EpubTidLists[1:3], "list") 


###################################################
### code chunk number 13: data
###################################################
data("AdultUCI")
dim(AdultUCI)
AdultUCI[1:2,]


###################################################
### code chunk number 14: arules.Rnw:1323-1325
###################################################
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL


###################################################
### code chunk number 15: arules.Rnw:1341-1356
###################################################
AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)),
    labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],
      c(0,25,40,60,168)),
    labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
			    
AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]],
      c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)),
    labels = c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
      c(-Inf,0,
	median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),
    labels = c("none", "low", "high"))


###################################################
### code chunk number 16: coerce
###################################################
Adult <- transactions(AdultUCI)
Adult


###################################################
### code chunk number 17: summary
###################################################
summary(Adult)


###################################################
### code chunk number 18: itemFrequencyPlot (eval = FALSE)
###################################################
## itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8)


###################################################
### code chunk number 19: arules.Rnw:1395-1396
###################################################
itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8)


###################################################
### code chunk number 20: apriori
###################################################
rules <- apriori(Adult, 
                 parameter = list(support = 0.01, confidence = 0.6))
rules


###################################################
### code chunk number 21: summary
###################################################
summary(rules)


###################################################
### code chunk number 22: rules
###################################################
rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift > 1.2)
rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift > 1.2)


###################################################
### code chunk number 23: subset
###################################################
inspect(head(rulesIncomeSmall, n = 3, by = "confidence"))
inspect(head(rulesIncomeLarge, n = 3, by = "confidence"))


###################################################
### code chunk number 24: write_rules (eval = FALSE)
###################################################
## write(rulesIncomeSmall, file = "data.csv", sep = ",", col.names = NA)


###################################################
### code chunk number 25: pmml (eval = FALSE)
###################################################
## write.PMML(rulesIncomeSmall, file = "data.xml")


###################################################
### code chunk number 26: arules.Rnw:1520-1523
###################################################
data("Adult")
fsets <- eclat(Adult, parameter = list(support = 0.05), 
	control = list(verbose=FALSE))


###################################################
### code chunk number 27: arules.Rnw:1531-1538
###################################################
singleItems <- fsets[size(items(fsets)) == 1]

## Get the col numbers we have support for
singleSupport <- quality(singleItems)$support
names(singleSupport) <- unlist(LIST(items(singleItems),
	    decode = FALSE))
head(singleSupport, n = 5)


###################################################
### code chunk number 28: arules.Rnw:1547-1554
###################################################
itemsetList <- LIST(items(fsets), decode = FALSE)

allConfidence <- quality(fsets)$support / 
    sapply(itemsetList, function(x) 
    max(singleSupport[as.character(x)]))

quality(fsets) <- cbind(quality(fsets), allConfidence)


###################################################
### code chunk number 29: arules.Rnw:1558-1559
###################################################
summary(fsets)


###################################################
### code chunk number 30: arules.Rnw:1567-1570
###################################################
fsetsEducation <- subset(fsets, subset = items %pin% "education")
inspect(sort(fsetsEducation[size(fsetsEducation)>1], 
	by = "allConfidence")[1 : 3])


###################################################
### code chunk number 31: arules.Rnw:1583-1585
###################################################
data("Adult")
Adult


###################################################
### code chunk number 32: arules.Rnw:1594-1600
###################################################
supp <- 0.05
epsilon <- 0.1
c <- 0.1

n <- -2 * log(c)/ (supp * epsilon^2)
n


###################################################
### code chunk number 33: arules.Rnw:1609-1610
###################################################
AdultSample <- sample(Adult, n, replace = TRUE)


###################################################
### code chunk number 34: itemFrequencyPlot2 (eval = FALSE)
###################################################
## itemFrequencyPlot(AdultSample, population = Adult, support = supp,
##     cex.names = 0.7)


###################################################
### code chunk number 35: arules.Rnw:1628-1629
###################################################
itemFrequencyPlot(AdultSample, population = Adult, support = supp,
    cex.names = 0.7)


###################################################
### code chunk number 36: itemFrequencyPlot3 (eval = FALSE)
###################################################
## itemFrequencyPlot(AdultSample, population = Adult, 
##     support = supp, lift = TRUE, 
##     cex.names = 0.9)


###################################################
### code chunk number 37: arules.Rnw:1657-1658
###################################################
itemFrequencyPlot(AdultSample, population = Adult, 
    support = supp, lift = TRUE, 
    cex.names = 0.9)


###################################################
### code chunk number 38: arules.Rnw:1669-1676
###################################################
time <- system.time(itemsets <- eclat(Adult, 
    parameter = list(support = supp), control = list(verbose = FALSE)))
time

timeSample <- system.time(itemsetsSample <- eclat(AdultSample, 
    parameter = list(support = supp), control = list(verbose = FALSE)))
timeSample


###################################################
### code chunk number 39: arules.Rnw:1685-1687
###################################################
# speed up
time[1] / timeSample[1]


###################################################
### code chunk number 40: arules.Rnw:1695-1697
###################################################
itemsets
itemsetsSample


###################################################
### code chunk number 41: arules.Rnw:1705-1708
###################################################
match <- match(itemsets, itemsetsSample, nomatch = 0)
## remove no matches
sum(match > 0) / length(itemsets)


###################################################
### code chunk number 42: arules.Rnw:1717-1719
###################################################
summary(quality(itemsets[match == 0])$support)
summary(quality(itemsetsSample[-match])$support)


###################################################
### code chunk number 43: arules.Rnw:1728-1734
###################################################
supportItemsets <- quality(itemsets[which(match > 0)])$support
supportSample <- quality(itemsetsSample[match])$support

accuracy <- 1 - abs(supportSample - supportItemsets) / supportItemsets

summary(accuracy)


