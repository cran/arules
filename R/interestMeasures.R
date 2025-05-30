#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta,
# 			Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


#' Calculate Additional Interest Measures
#'
#' Provides the generic function `interestMeasure()` and the
#' methods to calculate various additional interest measures for existing sets
#' of [itemsets] or [rules].
#'
#' A searchable list of definitions, equations and
#' references for all available interest measures can be found at
#' \url{https://mhahsler.github.io/arules/docs/measures}. The descriptions
#' are also linked in the list below.
#'
#' The following measures are implemented for **itemsets**:
#' `r arules:::create_measures_doc(arules:::measuresItemsets)`
#'
#' The following measures are implemented for **rules**:
#' `r arules:::create_measures_doc(arules:::measuresRules)`
#'
#' @aliases interestMeasure
#' @family interest measures
#'
#' @param x a set of [itemsets] or [rules].
#' @param measure name or vector of names of the desired interest measures (see
#' the Details section for available measures). If measure is missing then all available
#' measures are calculated.
#' @param transactions the [transactions] used to mine the associations
#' or a set of different transactions to calculate interest measures from
#' (Note: you need to set `reuse = FALSE` in the later case).
#' @param reuse logical indicating if information in the quality slot should be
#' reuse for calculating the measures. This speeds up the process significantly
#' since only very little (or no) transaction counting is necessary if support,
#' confidence and lift are already available. Use `reuse = FALSE` to force
#' counting (might be very slow but is necessary if you use a different set of
#' transactions than was used for mining).
#' @param ... further arguments for the measure calculation. Many measures
#' are based on contingency table counts and zero counts can produce `NaN` values
#' (division by zero). This issue can be resolved by using the additional
#' parameter `smoothCounts` which performs additive smoothing by adds a
#' "pseudo count" of `smoothCounts` to each cell in the contingency
#' table. Use `smoothCounts = 1` or larger values for Laplace smoothing.
#' Use `smoothCounts = .5` for Haldane-Anscombe correction (Haldan, 1940; Anscombe, 1956)
#' which is often used for chi-squared, phi correlation and related measures.
#'
#' @return If only one measure is used, the function returns a numeric vector
#' containing the values of the interest measure for each association in the
#' set of associations `x`.
#'
#' If more than one measures are specified, the result is a data.frame
#' containing the different measures for each association as columns.
#'
#' `NA` is returned for rules/itemsets for which a certain measure is not
#' defined.
#' @author Michael Hahsler
#' @seealso [itemsets], [rules]
#' @references
#' Hahsler, Michael (2015). A Probabilistic Comparison of Commonly Used
#' Interest Measures for Association Rules, 2015, URL:
#' \url{https://mhahsler.github.io/arules/docs/measures}.
#'
#' Haldane, J.B.S. (1940). "The mean and variance of the moments of chi-squared
#' when used as a test of homogeneity, when expectations are small".
#' _Biometrika,_ 29, 133-134.
#'
#' Anscombe, F.J. (1956). "On estimating binomial response relations".
#' _Biometrika,_ 43, 461-464.
#' @keywords models
#' @examples
#'
#' data("Income")
#' rules <- apriori(Income)
#'
#' ## calculate a single measure and add it to the quality slot
#' quality(rules) <- cbind(quality(rules),
#'   hyperConfidence = interestMeasure(rules,
#'     measure = "hyperConfidence",
#'     transactions = Income
#'   )
#' )
#'
#' inspect(head(rules, by = "hyperConfidence"))
#'
#' ## calculate several measures
#' m <- interestMeasure(rules, c("confidence", "oddsRatio", "leverage"),
#'   transactions = Income
#' )
#' inspect(head(rules))
#' head(m)
#'
#' ## calculate all available measures for the first 5 rules and show them as a
#' ## table with the measures as rows
#' t(interestMeasure(head(rules, 5), transactions = Income))
#'
#' ## calculate measures on a different set of transactions (I use a sample here)
#' ## Note: reuse = TRUE (default) would just return the stored support on the
#' ## 	data set used for mining
#' newTrans <- sample(Income, 100)
#' m2 <- interestMeasure(rules, "support", transactions = newTrans, reuse = FALSE)
#' head(m2)
#'
#' ## calculate all available measures for the 5 frequent itemsets with highest support
#' its <- apriori(Income, parameter = list(target = "frequent itemsets"))
#' its <- head(its, 5, by = "support")
#' inspect(its)
#'
#' interestMeasure(its, transactions = Income)
setGeneric(
  "interestMeasure",
  function(
      x,
      measure,
      transactions = NULL,
      reuse = TRUE,
      ...) {
    standardGeneric("interestMeasure")
  }
)


### only used to create the manual pages
addl_doc <- list(
  chisquared = "Additional parameters are: `significance = TRUE` returns the p-value of the test for independence instead of the chi-squared statistic. For p-values, substitution effects (the occurrence of one item makes the occurrence of another item less likely) can be tested using the parameter `complements = FALSE`.  Note: Correction for multiple comparisons can be done using [stats::p.adjust()].",
  fishersexacttest = "By default complementary effects are mined, substitutes can be found by using the parameter `complements = FALSE`. Note that Fisher's exact test is equal to hyper-confidence with `significance = TRUE`. Correction for multiple comparisons can be done using [stats::p.adjust()].",
  improvement = "The additional parameter `improvementMeasure` (default: `'confidence'`) can be used to specify the measure used for the improvement calculation. See [Generalized improvement](https://mhahsler.github.io/arules/docs/measures#generalizedImprovement).",
  hyperconfidence = "Reports the confidence level by default and the significance level if `significance = TRUE` is used. By default complementary effects are mined, substitutes (too low co-occurrence counts) can be found by using the parameter `complements = FALSE`.",
  hyperlift = "The used quantile can be changed using parameter `level` (default: `level = 0.99`).",
  laplace = "Parameter `k` can be used to specify the number of classes (default is 2).",
  lic = "The additional parameter `improvementMeasure` (default: `'lift'`) can be used to specify the measure used for the increase calculation.  See [Generalized increase ratio](https://mhahsler.github.io/arules/docs/measures#ginc).",
  stdLift = "By default, standardized lift is is corrected for minimum support and minimum confidence. Correction can be disabled by using the argument `correct = FALSE`.",
  table = "Returns the four counts for the contingency table. The entries are labeled `n11`, `n01`, `n10`, and `n00` (the first subscript is for X and the second is for Y; 1 indicated presence and 0 indicates absence). If several measures are specified, then the counts have the prefix `table.`"
)

create_measures_doc <- function(measures) {
  measures_doc <-
    grep("^## ", readLines("docs/measures.md"), value = TRUE)
  measures_doc <-
    do.call(
      rbind,
      strsplit(measures_doc, "\\s*\\{\\s*|##\\s*|#|\\s*\\}\\s*")
    )[, c(2, 4)]

  m <- match(tolower(measures), measures_doc[, 2])
  if (any(is.na(m))) {
    warning(
      "No matching entry for measure: ",
      paste(measures[is.na(m)], collapse = ", ")
    )
  }

  mm <- cbind(measures_doc[m, ], measures)

  paste0(
    '* "',
    mm[, 3],
    '": [',
    mm[, 1],
    ".](https://mhahsler.github.io/arules/docs/measures#",
    mm[, 2],
    ") ",
    sapply(
      tolower(mm[, 3]),
      FUN = function(m) {
        if (!is.null(addl_doc[[m]])) {
          addl_doc[[m]]
        } else {
          ""
        }
      }
    ),
    collapse = "\n"
  )
}

measuresItemsets <- c(
  "support",
  "count",
  "allConfidence",
  "crossSupportRatio",
  "lift"
)

#' @rdname interestMeasure
setMethod(
  "interestMeasure", signature(x = "itemsets"),
  function(
      x,
      measure,
      transactions = NULL,
      reuse = TRUE,
      ...) {
    ## backward comp.
    add <- list(...)
    if (!is.null(add$method)) {
      warning("interestMeasure: parameter method is now deprecated! Use measure instead!")
      measure <- add$method
    }

    if (missing(measure)) {
      measure <- measuresItemsets
    }

    ## check and expand measure
    if (any(is.na(ind <- pmatch(
      tolower(measure),
      tolower(measuresItemsets)
    )))) {
      stop(
        gettextf(
          "Invalid measure(s) for rules: %s",
          paste(measure[is.na(ind)], collapse = ", ")
        ),
        gettextf(
          "\n\n  Available measures: %s",
          paste(measuresItemsets, collapse = ", ")
        ),
        domain = NA
      )
    }

    measure <- measuresItemsets[ind]

    ## remove quality information if we do not want to reuse! Then we can start reusing
    if (!reuse) {
      quality(x) <- data.frame(seq_len(length(x)))[, 0]
    }

    if (is.null(quality(x)[["support"]])) {
      quality(x)[["support"]] <-
        support(x, transactions = transactions)
    }

    ## deal with multiple measures
    if (length(measure) > 1) {
      return(as.data.frame(
        sapply(
          measure,
          FUN =
            function(m) {
              interestMeasure(x, m, transactions, reuse = TRUE, ...)
            },
          USE.NAMES = TRUE,
          simplify = FALSE
        )
      ))
    }

    ## first see if we already have it:
    if (!is.null(quality(x)[[measure]])) {
      return(quality(x)[[measure]])
    }

    ## calculate measures
    if (measure == "count") {
      return(round(quality(x)[["support"]] * .getN(x, transactions)))
    }
    ## all other measures are basic measures
    return(.basicItemsetMeasures(x, measure, transactions, ...))
  }
)

.getN <- function(x, transactions) {
  ## find N from associations or if not available then from transactions
  n <- info(x)$ntransactions
  if (is.null(n)) {
    if (is.null(transactions)) {
      stop("transaction data needed. Please specify the transactions used to mine the itemsets!")
    }
    n <- length(transactions)
  }
  n
}

.basicItemsetMeasures <- function(
    x,
    measure,
    transactions = NULL,
    ...) {
  if (is.null(transactions)) {
    stop("transaction data needed. Please specify the transactions used to mine the itemsets!")
  }

  itemSupport <- itemFrequency(transactions)
  if (length(itemSupport) != nitems(items(x))) {
    stop("number of items in itemsets and transactions do not match.")
  }

  ## create an itemset list
  itemset_list <- LIST(items(x), decode = FALSE)

  ## catch empty itemset
  if (length(itemset_list) < 1) {
    return(numeric(0))
  }

  ## calculate all-confidence for existing (frequent) itemsets.
  ##
  ## Edward R. Omiecinski. Alternative interest measures for mining
  ## associations in databases. IEEE Transactions on Knowledge and
  ## Data Engineering, 15(1):57-69, Jan/Feb 2003.
  ##
  ## calculate all-confidence using itemsets support and the
  ## singleton support of the most frequent item in the itemset
  ## all-confidence(Z) = supp(Z) / max(supp(i elem Z))

  if (measure == "allConfidence") {
    m <-
      quality(x)[["support"]] / sapply(itemset_list, function(i) {
        max(itemSupport[i])
      })

    ### deal with 1-itemsets
    is1 <- size(x) == 1
    m[is1] <- quality(x)[["support"]][is1]
  }

  ## calculate the cross-support ratio
  ## used to eliminate cross support patterns which contain item with
  ## extremely different support. These patterns tend to be spurious
  ## (i.e., one item which occurs in virtually all transactions and some very
  ##  rare items)
  ##
  ## Hui Xiong, Pang-Ning Tan, Vipin Kumar. Mining Strong Affinity Association
  ## Patterns in Data Sets with Skewed Support. Third IEEE International
  ## Conference on Data Mining, Melbourne, Florida, November 19 - 22, 2003.
  ##

  if (measure == "crossSupportRatio") {
    m <- sapply(itemset_list, function(i) {
      min(itemSupport[i])
    }) /
      sapply(itemset_list, function(i) {
        max(itemSupport[i])
      })
  }

  if (measure == "lift") {
    m <-
      quality(x)[["support"]] / sapply(itemset_list, function(i) {
        prod(itemSupport[i])
      })
  }

  m[!is.finite(m)] <- NA

  return(m)
}

## measures for rules

# sort measures (except 1-4)
# dput(c(measuresRules[1:4], sort(measuresRules[-(1:4)])))
measuresRules <-
  c(
    "support",
    "confidence",
    "lift",
    "count",
    "addedValue",
    "boost",
    "casualConfidence",
    "casualSupport",
    "centeredConfidence",
    "certainty",
    "chiSquared",
    "collectiveStrength",
    "confirmedConfidence",
    "conviction",
    "cosine",
    "counterexample",
    "coverage",
    "doc",
    "fishersExactTest",
    "gini",
    "hyperConfidence",
    "hyperLift",
    "imbalance",
    "implicationIndex",
    "importance",
    "improvement",
    "jaccard",
    "jMeasure",
    "kappa",
    "kulczynski",
    "lambda",
    "laplace",
    "leastContradiction",
    "lerman",
    "leverage",
    "LIC",
    "maxconfidence",
    "mutualInformation",
    "oddsRatio",
    "phi",
    "ralambondrainy",
    "relativeRisk",
    "rhsSupport",
    "RLD",
    "rulePowerFactor",
    "sebag",
    "stdLift",
    "table",
    "varyingLiaison",
    "yuleQ",
    "yuleY"
  )


#' @rdname interestMeasure
setMethod(
  "interestMeasure", signature(x = "rules"),
  function(
      x,
      measure,
      transactions = NULL,
      reuse = TRUE,
      ...) {
    ## backward comp.
    add <- list(...)
    if (!is.null(add$method)) {
      warning("interestMeasure: parameter method is now deprecated! Use measure instead!")
      measure <- add$method
    }

    if (missing(measure)) {
      measure <- measuresRules
    }

    ## check and expand measure
    if (any(is.na(ind <- pmatch(
      tolower(measure),
      tolower(measuresRules)
    )))) {
      stop(
        gettextf(
          "Invalid measure(s) for rules: %s",
          paste(measure[is.na(ind)], collapse = ", ")
        ),
        gettextf(
          "\n\n  Available measures: %s",
          paste(measuresRules, collapse = ", ")
        ),
        domain = NA
      )
    }

    measure <- measuresRules[ind]

    ## remove quality information if we do not want to reuse! Then we can start reusing
    if (!reuse) {
      quality(x) <- data.frame(seq_len(length(x)))[, 0]
    }

    ## precalculate some measures once (most measures can be calculated using support, confidence, and lift)
    ## if we haive no support then we probably have nothing! Count it with a single p-tree
    if (is.null(quality(x)[["support"]])) {
      s <-
        support(c(items(x), lhs(x), rhs(x)), transactions = transactions)
      quality(x)[["support"]] <- s[seq(length(x))]
      quality(x)[["coverage"]] <- s[length(x) + seq(length(x))]
      quality(x)[["confidence"]] <-
        quality(x)[["support"]] / quality(x)[["coverage"]]
      quality(x)[["rhsSupport"]] <-
        s[2 * length(x) + seq(length(x))]
      quality(x)[["lift"]] <-
        quality(x)[["confidence"]] / quality(x)[["rhsSupport"]]
    }

    if (is.null(quality(x)[["coverage"]])) {
      quality(x)[["coverage"]] <-
        coverage(x, transactions = transactions)
    }
    if (is.null(quality(x)[["confidence"]])) {
      quality(x)[["confidence"]] <-
        quality(x)[["support"]] / quality(x)[["coverage"]]
    }
    if (is.null(quality(x)[["lift"]])) {
      quality(x)[["lift"]] <-
        quality(x)[["confidence"]] / .rhsSupport(x, transactions = transactions)
    }

    if (length(measure) > 1L) {
      return(as.data.frame(
        sapply(
          measure,
          FUN =
            function(m) {
              interestMeasure(x, m, transactions = transactions, reuse = TRUE, ...)
            },
          USE.NAMES = TRUE,
          simplify = FALSE
        )
      ))
    }

    ## catch empty ruleset
    if (length(x) < 1) {
      return(numeric(0))
    }

    ## first see if we already have a basic measure. All others we recalculate.
    if (measure %in% c("support", "confidence", "coverage", "lift") &&
      !is.null(quality(x)[[measure]])) {
      return(quality(x)[[measure]])
    }

    ## calculate measure (support, confidence, lift and coverage are already handled)
    if (measure == "boost") {
      return(.conf_boost(x, transactions = transactions, ...))
    }
    if (measure == "count") {
      return(round(quality(x)[["support"]] * .getN(x, transactions)))
    }
    if (measure == "rhsSupport") {
      return(.rhsSupport(x, transactions))
    }
    if (measure == "improvement") {
      return(.improvement(x, transactions = transactions, ...))
    }
    if (measure == "LIC") {
      return(.lic(x, transactions = transactions, ...))
    }
    if (measure == "hyperLift") {
      return(.hyperLift(x, transactions = transactions, ...))
    }
    if (measure == "hyperConfidence") {
      return(.hyperConfidence(x, transactions = transactions, ...))
    }
    if (measure == "fishersExactTest") {
      return(.hyperConfidence(x,
        transactions = transactions,
        significance = TRUE,
        ...
      ))
    }
    if (measure == "RLD") {
      return(.RLD(x, transactions = transactions, ...))
    }
    if (measure == "stdLift") {
      return(.stdLift(x, transactions = transactions, ...))
    }

    ## all other measures are implemented here (i is in ...)
    ret <-
      .basicRuleMeasure(x, measure, transactions = transactions, ...)

    ## make all bad values NA (does not work for measures that return data.frames)
    # if (is.vector(ret)) ret[!is.finite(ret)] <- NA

    return(ret)

    stop("Specified measure not implemented.")
  }
)

## calculate hyperlift for existing rules.
##
## Michael Hahsler, Kurt Hornik, and Thomas Reutterer.
## Implications of probabilistic data modeling for rule mining.
## Report 14, Research Report Series, Department of Statistics and
## Mathematics, Wirtschaftsuniversitaet Wien, Augasse 2-6, 1090 Wien,
## Austria, March 2005.

## hyperlift(X => Y) = c_X,Y / Q_d[C_X,Y]
##
## where Q_d[C_X,Y] = qhyper(d, m = c_Y, n = length(trans.) - c_Y, k = c_X)
##
## c_X,Y = count(X => Y)
## c_X = count(X)
## c_Y = count(Y)
##
## this implements only hyperlift for rules with a single item in the consequent

.hyperLift <- function(x, level = 0.99, ...) {
  counts <- .getCounts(x, ...)

  with(counts, {
    Q <-
      stats::qhyper(
        level,
        m = nx1,
        n = n - nx1,
        k = n1x,
        lower.tail = TRUE
      )
    n11 / Q
  })
}


## calculate hyperconfidence for existing rules.
## (confidence level that we observe too high/low counts)
##
## uses the model from:
## Hahsler, Michael and Kurt Hornik (2007). New probabilistic
## interest measures for association rules.
## Intelligent Data Analysis, 11(5):437--455.

.hyperConfidence <-
  function(
      x,
      complements = TRUE,
      significance = FALSE,
      ...) {
    ## significance: return significance levels instead of
    ##   confidence levels

    counts <- .getCounts(x, ...)


    if (complements == TRUE) {
      ## c_XY - 1 so we get P[C_XY < c_XY] instead of P[C_XY <= c_XY]
      res <- with(counts, {
        stats::phyper(
          n11 - 1,
          m = nx1,
          n = n - nx1,
          k = n1x,
          lower.tail = !significance
        )
      })
    } else {
      ## substitutes; Pr[C_XY > c_XY]
      ## empty LHS causes a div by zero -> NAN
      suppressWarnings(res <- with(counts, {
        stats::phyper(
          n11,
          m = nx1,
          n = n - n1x,
          k = n1x,
          lower.tail = significance
        )
      }))
    }

    res[is.nan(res)] <- NA
    res
  }

## Minimum Improvement (Bayardo et al. 1999)
## Let the improvement of a rule be defined as the minimum
## difference between its confidence and the confidence of any
## proper sub-rule with the same consequent.

.improvement <- function(
    x,
    improvementMeasure = "confidence", ...) {
  ## Note: improvement is defined for confidence, but could also used with
  ## other measures
  q <- interestMeasure(x, measure = improvementMeasure, ...)
  imp <- numeric(length(x))

  ### do it by unique rhs
  rr <- .Call(R_pnindex, rhs(x)@data, NULL, FALSE)

  for (r in unique(rr)) {
    pos <- which(rr == r)

    q2 <- q[pos]
    ### FALSE is for verbose
    qsubmax <- .Call(R_pnmax, lhs(x[pos])@data, q2, FALSE)

    imp[pos] <- q2 - qsubmax
  }

  imp
}

.conf_boost <- function(x, ...) {
  conf <- interestMeasure(x, "confidence", ...)
  imp <- .improvement(x, ...)

  conf / (conf - imp)
}

## LIC (Lift Increase) (Lopez 2014)
## Like improvement but divides instead of subtracts

.lic <- function(x,
                 improvementMeasure = "lift", ...) {
  ## Note: LIC is defined for lift, but could also used with
  ## other measures
  q <- interestMeasure(x, measure = improvementMeasure, ...)
  imp <- numeric(length(x))

  ### do it by unique rhs
  rr <- .Call(R_pnindex, rhs(x)@data, NULL, FALSE)

  for (r in unique(rr)) {
    pos <- which(rr == r)

    q2 <- q[pos]
    ### FALSE is for verbose
    qsubmax <- .Call(R_pnmax, lhs(x[pos])@data, q2, FALSE)

    imp[pos] <- q2 / qsubmax
  }

  imp
}


.getCounts <-
  function(
      x,
      transactions = NULL,
      reuse = TRUE,
      smoothCounts = 0) {
    if (smoothCounts < 0) {
      stop("smoothCount needs to be >= 0!")
    }

    q <-
      interestMeasure(
        x,
        c("support", "coverage", "rhsSupport"),
        transactions = transactions,
        reuse = reuse
      )

    n <- .getN(x, transactions)
    n11 <- round(q$support * n)
    n1x <- round(q$coverage * n)
    nx1 <- round(q$rhsSupport * n)
    n0x <- n - n1x
    nx0 <- n - nx1
    n10 <- n1x - n11
    n01 <- nx1 - n11
    n00 <- n0x - n01

    if (smoothCounts > 0) {
      n <- n + 4 * smoothCounts
      n11 <- n11 + smoothCounts
      n10 <- n10 + smoothCounts
      n01 <- n01 + smoothCounts
      n00 <- n00 + smoothCounts

      n0x <- n0x + 2 * smoothCounts
      nx0 <- nx0 + 2 * smoothCounts
      n1x <- n1x + 2 * smoothCounts
      nx1 <- nx1 + 2 * smoothCounts
    }


    list(
      n11 = n11,
      n01 = n01,
      n10 = n10,
      n00 = n00,
      n1x = n1x,
      nx1 = nx1,
      n0x = n0x,
      nx0 = nx0,
      n = n
    )
  }

.rhsSupport <- function(x, transactions) {
  q <- quality(x)

  if (!is.null(q$confidence) && !is.null(q$lift)) {
    rhsSupport <- q$confidence / q$lift
    ### in case lift was NaN (0/0)
    rhsSupport[is.na(rhsSupport)] <- 0
  } else {
    if (is.null(transactions)) {
      stop(
        "transactions missing. Please specify the data used to mine the rules as transactions!"
      )
    }
    if (all(diff(rhs(x)@data@p) == 1)) {
      ### this is a lot faster for single items in the RHS
      rhsSupport <-
        unname(itemFrequency(transactions)[rhs(x)@data@i + 1L])
    } else {
      rhsSupport <-
        support(rhs(x), transactions)
    } ### multiple items in the RHS
  }

  return(rhsSupport)
}


## More measures (see Tan et al. Introduction to Data Mining, 2006)
# x can be a set of rules or list with counts (at least n11, n10, n01, n11 and n)

.basicRuleMeasure <- function(
    x,
    measure,
    transactions = NULL,
    ### adds smoothCounts to the count in each cell to avoid counts of 0
    smoothCounts = 0,
    ### k is the number of classes used by laplace
    significance = FALSE,
    ### used by chi-squared
    complement = TRUE,
    ### k is the number of classes used by laplace
    k = 2) {
  if (is(x, "rules")) {
    counts <-
      .getCounts(x, transactions, smoothCounts = smoothCounts)
  } else {
    # is counts a matrix?
    if (is.matrix(x)) {
      counts <- lapply(seq_len(ncol(x)), function(i) {
        x[, i]
      })
      names(counts) <- colnames(x)
    } else {
      counts <- x
    }

    # complete missing counts if x has counts
    if (is.null(counts$n)) {
      counts$n <- counts$n11 + counts$n10 + counts$n01 + counts$n00
    }
    if (is.null(counts$n1x)) {
      counts$n1x <- counts$n11 + counts$n10
    }
    if (is.null(counts$nx1)) {
      counts$nx1 <- counts$n11 + counts$n01
    }
    if (is.null(counts$n0x)) {
      counts$n0x <- counts$n - counts$n1x
    }
    if (is.null(counts$nx0)) {
      counts$nx0 <- counts$n - counts$nx1
    }
  }


  # note return in with just assigns to m
  m <- with(
    counts,
    switch(measure,
      table = data.frame(
        n11 = n11,
        n01 = n01,
        n10 = n10,
        n00 = n00
      ),
      support = n11 / n,
      confidence = n11 / n1x,
      lift = n * n11 / (n1x * nx1),
      coverage = n1x / n,
      rhsSupport = nx1 / n,
      cosine = n11 / sqrt(n1x * nx1),
      conviction = n1x * nx0 / (n * n10),
      gini = n1x / n * ((n11 / n1x)^2 + (n10 / n1x)^2) - (nx1 / n)^2 +
        n0x / n * ((n01 / n0x)^2 + (n00 / n0x)^2) - (nx0 / n)^2,
      rulePowerFactor = n11 * n11 / n1x / n,
      oddsRatio = n11 * n00 / (n10 * n01),
      relativeRisk = (n11 / n1x) / (n01 / n0x),
      phi = (n * n11 - n1x * nx1) / sqrt(n1x * nx1 * n0x * nx0),
      leverage = n11 / n - (n1x * nx1 / n^2),
      collectiveStrength = n11 * n00 / (n1x * nx1 + n0x + nx0) *
        (n^2 - n1x * nx1 - n0x * nx0) / (n - n11 - n00),
      importance = log(((n11 + 1) * (n0x + 2)) / ((n01 + 1) * (n1x + 2)), base = 10),
      imbalance = abs(n1x - nx1) / (n1x + nx1 - n11),
      jaccard = n11 / (n1x + nx1 - n11),
      kappa = (n * n11 + n * n00 - n1x * nx1 - n0x * nx0) / (n^2 - n1x * nx1 -
        n0x * nx0),
      lambda = {
        max_x0x1 <- apply(cbind(nx1, nx0), 1, max)
        (apply(cbind(n11, n10), 1, max) + apply(cbind(n01, n00), 1, max) -
          max_x0x1) / (n - max_x0x1)
      },
      mutualInformation = (
        n00 / n * log(n * n00 / (n0x * nx0)) +
          n01 / n * log(n * n01 / (n0x * nx1)) +
          n10 / n * log(n * n10 / (n1x * nx0)) +
          n11 / n * log(n * n11 / (n1x * nx1))
      ) /
        pmin(-1 * (n0x / n * log(n0x / n) + n1x / n * log(n1x / n)), -1 *
          (nx0 / n * log(nx0 / n) + nx1 / n * log(nx1 / n))),
      maxconfidence = pmax(n11 / n1x, n11 / nx1),
      jMeasure = n11 / n * log(n * n11 / (n1x * nx1)) +
        n10 / n * log(n * n10 / (n1x * nx0)),
      kulczynski = (n11 / n1x + n11 / nx1) / 2,
      laplace = (n11 + 1) / (n1x + k),
      certainty = (n11 / n1x - nx1 / n) / (1 - nx1 / n),
      addedValue = n11 / n1x - nx1 / n,
      ralambondrainy = n10 / n,
      sebag = (n1x - n10) / n10,
      counterexample = (n11 - n10) / n11,
      # needs alpha
      # if(measure == "wang") return(1/n * (1-alpha) * n1x - n10)
      confirmedConfidence = (n11 - n10) / n1x,
      casualSupport = (n1x + nx1 - 2 * n10) / n,
      casualConfidence = 1 - n10 / n * (1 / n1x + 1 / nx1),
      leastContradiction = (n1x - n10) / nx1,
      centeredConfidence = nx0 / n - n10 / n1x,
      varyingLiaison = (n1x - n10) / (n1x * nx1 / n) - 1,
      yuleQ = {
        OR <- n11 * n00 / (n10 * n01)
        (OR - 1) / (OR + 1)
      },
      yuleY = {
        OR <- n11 * n00 / (n10 * n01)
        (sqrt(OR) - 1) / (sqrt(OR) + 1)
      },
      lerman = (n11 - n1x * nx1 / n) / sqrt(n1x * nx1 / n),
      implicationIndex = (n10 - n1x * nx0 / n) / sqrt(n1x * nx0 / n),
      doc = (n11 / n1x) - (n01 / n0x),
      chiSquared = {
        chi2 <- numeric(length(x))

        for (i in seq_len(length(x))) {
          fo <- matrix(c(n00[i], n01[i], n10[i], n11[i]), ncol = 2)
          # fe <- tcrossprod(c(nx0[i], nx1[i]), ic(n0x[i], n1x[i])) / n
          ## check if approximation is ok
          ## we don't do this now
          ## if(any(fe < 5)) chi2[i] <- nA
          ## else
          # chi2[i] <- sum((fo - fe) ^ 2 / fe)

          # warning about approximation
          suppressWarnings(chi2[i] <-
            stats::chisq.test(fo, correct = FALSE)$statistic)
        }

        ## the chi square test has 1 df for a 2x2 contingency table.
        ## The critical value at alpha=0.05 is:
        ## qchisq(0.05, df =1, lower.tail=FALSE)
        ## [1] 3.841459
        if (!significance) {
          chi2
        } else {
          stats::pchisq(
            q = chi2,
            df = 1,
            lower.tail = !complement
          )
        }
      }
    )
  )

  if (is.null(m)) {
    stop("Specified measure not implemented.")
  }

  m
}


## RLD see Kenett and Salini 2008
## RLD code contributed by Silvia Salini

.RLD <- function(x, ...) {
  counts <- .getCounts(x, ...)
  RLD <- with(counts, {
    RLD <- numeric(length(x))
    for (i in seq_len(length(x))) {
      D <- (n11[i] * n00[i] - n10[i] * n01[i]) / n
      if (D > 0) {
        if (n01[i] < n10[i]) {
          RLD[i] <- D / (D + n01[i])
        } else {
          RLD[i] <- D / (D + n10[i])
        }
      } else if (n11[i] < n00[i]) {
        RLD[i] <- D / (D - n11[i])
      } else {
        RLD[i] <- D / (D - n00[i])
      }
    }
    RLD
  })

  RLD[!is.finite(RLD)] <- NA

  RLD
}

## Standardising the Lift of an Association Rule
# by McNicholas, 2008, DOI: 10.1016/j.csda.2008.03.013

.stdLift <-
  function(
      rules,
      transactions = NULL,
      correct = TRUE) {
    measures <- interestMeasure(rules,
      c("support", "confidence", "lift", "coverage", "rhsSupport"),
      transactions = transactions
    )

    n <- info(rules)$ntransactions
    if (is.null(n)) {
      if (is.null(transactions)) {
        stop("rules do not contain info from transactions. transactions are needed.")
      }
      n <- length(transactions)
    }

    supp_A <- measures$coverage
    supp_B <- measures$rhsSupport

    # upper bound of lift
    lambda <- pmax(supp_A + supp_B - 1, 1 / n) / (supp_A * supp_B)

    # correct lambda for min. confidence and min. support
    if (correct) {
      c <- info(rules)$confidence
      s <- info(rules)$support

      if (!is.null(c) && !is.null(s)) {
        lambda <-
          pmax(lambda, 4 * s / (1 + s)^2, s / (supp_A * supp_B), c / supp_B)
      } else {
        warning(
          "minimum support or confidence not available in info(x). Using uncorrected stdLift instead."
        )
      }
    }

    # lower bound of lift
    upsilon <- 1 / pmax(supp_A, supp_B)

    stdLift <- (measures$lift - lambda) / (upsilon - lambda)
    stdLift[is.nan(stdLift)] <- 1
    stdLift
  }
