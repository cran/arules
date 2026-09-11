## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(arules)
set.seed(1234)

## -----------------------------------------------------------------------------
trans <- transactions(list(
  T1 = c("apple", "banana"),
  T2 = c("apple", "yogurt"),
  T3 = c("banana", "milk"),
  T4 = c("apple", "banana", "milk"),
  T5 = c("milk", "yogurt"),
  T6 = c("apple", "banana", "yogurt")
))
itemInfo(trans)

## -----------------------------------------------------------------------------
category_list <- c(
  apple = "fruit", banana = "fruit",
  milk = "dairy", yogurt = "dairy"
)
itemInfo(trans)$category <- category_list[itemLabels(trans)]
itemInfo(trans)

## -----------------------------------------------------------------------------
by_category <- aggregate(trans, by = "category")
inspect(trans)
inspect(by_category)
itemFrequency(by_category)

## -----------------------------------------------------------------------------
category_rules <- apriori(
  by_category,
  parameter = list(support = 0.3, confidence = 0.5, minlen = 2),
  control = list(verbose = FALSE)
)
inspect(category_rules)

## -----------------------------------------------------------------------------
multilevel <- addAggregate(trans, by = "category")
inspect(multilevel)

multilevel_rules <- apriori(
  multilevel,
  parameter = list(support = 0.1, confidence = 0.6, minlen = 2),
  control = list(verbose = FALSE)
)
multilevel_rules

## -----------------------------------------------------------------------------
multilevel_rules <- filterAggregate(multilevel_rules)
multilevel_rules
  
inspect(sort(multilevel_rules, by = "lift"))

