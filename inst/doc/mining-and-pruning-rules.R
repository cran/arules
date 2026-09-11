## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(arules)
set.seed(1234)

## -----------------------------------------------------------------------------
trans <- transactions(list(
  T1 = c("bread", "butter", "milk"),
  T2 = c("bread", "butter"),
  T3 = c("bread", "milk"),
  T4 = c("bread", "butter", "jam"),
  T5 = c("bread", "butter", "milk"),
  T6 = c("butter", "jam"),
  T7 = c("bread", "milk", "cereal"),
  T8 = c("bread", "butter", "jam")
))

## -----------------------------------------------------------------------------
rules <- apriori(
  trans,
  parameter = list(
    support = 0.25, confidence = 0.6,
    maxlen = 3
  ),
  appearance = list(
    rhs = c("butter", "milk"),
    default = "lhs"
  )
)
inspect(rules)

## -----------------------------------------------------------------------------
selected <- subset(rules, lift > 1 & confidence >= 0.7)
ranked <- sort(selected, by = "lift", decreasing = TRUE)
inspect(ranked)

## -----------------------------------------------------------------------------
non_redundant <- rules[!is.redundant(rules)]
inspect(sort(non_redundant, by = "lift"))

## -----------------------------------------------------------------------------
inspect(rules[is.redundant(rules)])

