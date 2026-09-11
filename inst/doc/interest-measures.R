## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(arules)
set.seed(1234)

## -----------------------------------------------------------------------------
trans <- transactions(list(
  T1 = c("tea", "cookies", "milk"),
  T2 = c("tea", "cookies"),
  T3 = c("coffee", "cookies"),
  T4 = c("tea", "milk"),
  T5 = c("coffee", "milk"),
  T6 = c("tea", "cookies", "milk"),
  T7 = c("coffee", "cookies"),
  T8 = c("tea", "cookies")
))

rules <- apriori(
  trans,
  parameter = list(support = 0.25, confidence = 0.5),
  control = list(verbose = FALSE)
)

## -----------------------------------------------------------------------------
head(quality(rules))

## -----------------------------------------------------------------------------
measures <- interestMeasure(
  rules,
  measure = c("leverage", "phi"),
  transactions = trans
)
head(measures)

## -----------------------------------------------------------------------------
quality(rules) <- cbind(
  quality(rules),
  interestMeasure(
    rules,
    measure = c("leverage", "phi"),
    transactions = trans
  )
)

## -----------------------------------------------------------------------------
inspect(head(sort(rules, by = "leverage"), 3))

