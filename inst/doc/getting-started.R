## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(arules)
set.seed(1234)

## ----install, eval=FALSE------------------------------------------------------
# install.packages("arules")

## ----load-package-------------------------------------------------------------
library(arules)

## -----------------------------------------------------------------------------
baskets <- list(
  T1 = c("milk", "bread", "butter"),
  T2 = c("bread", "butter"),
  T3 = c("milk", "bread"),
  T4 = c("bread", "jam"),
  T5 = c("milk", "bread", "butter"),
  T6 = c("beer", "chips"),
  T7 = c("beer", "chips", "salsa"),
  T8 = c("bread", "butter", "jam")
)
trans <- transactions(baskets)
trans
inspect(trans[1:3])

## -----------------------------------------------------------------------------
summary(trans)
sort(itemFrequency(trans), decreasing = TRUE)

## -----------------------------------------------------------------------------
rules <- apriori(
  trans,
  parameter = list(support = 0.25, confidence = 0.6, maxlen = 5),
  control = list(verbose = FALSE)
)
rules

## -----------------------------------------------------------------------------
inspect(sort(rules, by = "lift"))

## -----------------------------------------------------------------------------
butter_rules <- subset(rules, rhs %in% "butter" & lift > 1)
inspect(butter_rules)

