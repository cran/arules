## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(arules)
set.seed(1234)

## -----------------------------------------------------------------------------
baskets <- list(
  order_1 = c("apple", "bread"),
  order_2 = c("bread", "milk"),
  order_3 = c("apple", "bread", "milk")
)
from_list <- transactions(baskets)
inspect(from_list)

## -----------------------------------------------------------------------------
summary(from_list)
itemLabels(from_list)

## -----------------------------------------------------------------------------
binary <- matrix(
  c(TRUE, TRUE, FALSE,
    FALSE, TRUE, TRUE,
    TRUE, TRUE, TRUE),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(names(baskets), c("apple", "bread", "milk"))
)
from_matrix <- transactions(binary)

itemLabels(from_matrix)
inspect(from_matrix)

## -----------------------------------------------------------------------------
customers <- data.frame(
  age_group = factor(c("young", "adult", "adult")),
  region = factor(c("north", "south", "north")),
  subscriber = c(TRUE, FALSE, TRUE)
)
from_wide <- transactions(customers)

itemLabels(from_wide)
inspect(from_wide)

## -----------------------------------------------------------------------------
measurements <- data.frame(
  spend = c(12, 18, 35, 42, 55),
  visits = c(1, 2, 3, 5, 8)
)
measurements_discrete <- discretizeDF(
  measurements,
  default = list(method = "frequency", breaks = 2)
)
from_discrete <- transactions(measurements_discrete)

itemLabels(from_discrete)
inspect(from_discrete)

## -----------------------------------------------------------------------------
long <- data.frame(
  order = c(1, 1, 2, 2, 3),
  product = c("apple", "bread", "bread", "milk", "apple")
)
from_long <- transactions(long, format = "long", cols = c("order", "product"))

itemLabels(from_long)
inspect(from_long)

