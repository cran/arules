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


#' Find Redundant Rules
#'
#' Provides the generic function `is.redundant()` and the method to find
#' redundant rules based on any interest measure.
#'
#' \bold{Simple improvement-based redundancy:} (`confint = FALSE`) A rule
#' can be defined as redundant if a more general rules with the same or a
#' higher confidence exists. That is, a more specific rule is redundant if it
#' is only equally or even less predictive than a more general rule. A rule is
#' more general if it has the same RHS but one or more items removed from the
#' LHS. Formally, a rule \eqn{X \Rightarrow Y}{X -> Y} is redundant if
#'
#' \deqn{\exists X' \subset X \quad conf(X' \Rightarrow Y) \ge conf(X
#' \Rightarrow Y).}{ for some X' subset X, conf(X' -> Y) >= conf(X -> Y).}
#'
#' This is equivalent to a negative or zero \emph{improvement} as defined by
#' Bayardo et al. (2000).
#'
#' The idea of improvement can be extended other measures besides confidence.
#' Any other measure available for function [interestMeasure()] (e.g.,
#' lift or the odds ratio) can be specified in `measure`.
#'
#' \bold{Confidence interval-based redundancy:} (`confint = TRUE`) Li et
#' al (2014) propose to use the confidence interval (CI) of the odds ratio (OR)
#' of rules to define redundancy. A more specific rule is redundant if it does
#' not provide a significantly higher OR than any more general rule. Using
#' confidence intervals as error bounds, a more specific rule is
#' defined as redundant if its OR CI overlaps with the CI of any more general
#' rule. This type of redundancy detection removes more rules
#' than improvement since it takes differences in counts due to randomness in
#' the dataset into account.
#'
#' The odds ratio and the CI are based on counts which can be zero and which
#' leads to numerical problems. In addition to the method described by Li et al
#' (2014), we use additive smoothing (Laplace smoothing) to alleviate this
#' problem. The default setting adds 1 to each count (see
#' [confint()]). A different pseudocount (smoothing parameter) can be
#' defined using the additional parameter `smoothCounts`. Smoothing can be
#' disabled using `smoothCounts = 0`.
#'
#' **Warning:** This approach of redundancy checking is flawed since rules with
#' non-overlapping CIs are
#' non-redundant (same result as for a 2-sample t-test), but overlapping CIs do
#' not automatically mean that there is no significant difference between the
#' two measures which leads to a higher type II error. At the same time,
#' multiple comparisons are performed leading to an increased type I error. If
#' we are more worried about missing important rules, then the type II error
#' is more concerning.
#'
#' Confidence interval-based redundancy checks can also be used for other
#' measures with a confidence interval like confidence (see
#' [confint()]).
#'
#' @family postprocessing
#' @family associations functions
#' @family interest measures
#'
#' @param x a set of rules.
#' @param measure measure used to check for redundancy.
#' @param confint should confidence intervals be used to the redundancy check?
#' @param level confidence level for the confidence interval. Only used when
#' `confint = TRUE`.
#' @param smoothCounts adds a "pseudo count" to each count in the used
#' contingency table. This implements addaptive smoothing (Laplace smoothing)
#' for counts and avoids zero counts.
#' @param ...  additional arguments are passed on to
#' [interestMeasure()], or, for `confint = TRUE` to
#' [confint()].
#' @return returns a logical vector indicating which rules are redundant.
#' @author Michael Hahsler and Christian Buchta
#' @references Bayardo, R. , R. Agrawal, and D. Gunopulos (2000).
#' Constraint-based rule mining in large, dense databases. \emph{Data Mining
#' and Knowledge Discovery,} 4(2/3):217--240.
#'
#' Li, J., Jixue Liu, Hannu Toivonen, Kenji Satou, Youqiang Sun, and Bingyu Sun
#' (2014). Discovering statistically non-redundant subgroups. Knowledge-Based
#' Systems. 67 (September, 2014), 315--327.
#' \doi{10.1016/j.knosys.2014.04.030}
#' @keywords manip
#' @examples
#'
#' data("Income")
#'
#' ## mine some rules with the consequent "language in home=english"
#' rules <- apriori(Income,
#'   parameter = list(support = 0.5),
#'   appearance = list(rhs = "language in home=english")
#' )
#'
#' ## for better comparison we add Bayado's improvement and sort by improvement
#' quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")
#' rules <- sort(rules, by = "improvement")
#' inspect(rules)
#' is.redundant(rules)
#'
#' ## find non-redundant rules using improvement of confidence
#' ## Note: a few rules have a very small improvement over the rule {} => {language in home=english}
#' rules_non_redundant <- rules[!is.redundant(rules)]
#' inspect(rules_non_redundant)
#'
#' ## use non-overlapping confidence intervals for the confidence measure instead
#' ## Note: fewer rules have a significantly higher confidence
#' inspect(rules[!is.redundant(rules,
#'   measure = "confidence",
#'   confint = TRUE, level = 0.95
#' )])
#'
#' ## find non-redundant rules using improvement of the odds ratio.
#' quality(rules)$oddsRatio <- interestMeasure(rules, measure = "oddsRatio", smoothCounts = .5)
#' inspect(rules[!is.redundant(rules, measure = "oddsRatio")])
#'
#' ## use the confidence interval for the odds ratio.
#' ## We see that no rule has a significantly better odds ratio than the most general rule.
#' inspect(rules[!is.redundant(rules,
#'   measure = "oddsRatio",
#'   confint = TRUE, level = 0.95
#' )])
#'
#' ##  use the confidence interval for lift
#' inspect(rules[!is.redundant(rules,
#'   measure = "lift",
#'   confint = TRUE, level = 0.95
#' )])
#'
setGeneric(
  "is.redundant",
  function(x, ...) {
    standardGeneric("is.redundant")
  }
)

#' @rdname is.redundant
setMethod(
  "is.redundant", signature(x = "rules"),
  function(
      x,
      measure = "confidence",
      confint = FALSE,
      level = 0.95,
      smoothCounts = 1,
      ...) {
    if (confint) {
      .improvementCI(x,
        measure = measure,
        level = level,
        smoothCounts = smoothCounts,
        ...
      ) <= 0
    } else {
      interestMeasure(
        x,
        measure = "improvement",
        improvementMeasure = measure,
        smoothCounts = smoothCounts,
        ...
      ) <= 0
    }
  }
)

# Is the supersets oddsRatio sign. larger than all its subsets?
# I.e., the superset's lower bound needs to be larger than the subset's upper bound

.improvementCI <- function(
    x,
    measure,
    level = 0.95,
    smoothCounts = 0.5,
    ...) {
  q <- confint(x,
    measure,
    level = level,
    smoothCounts = smoothCounts,
    ...
  )

  # q is a data.frame with LL and UL
  imp <- numeric(length(x))

  ### do it by unique rhs
  rr <- .Call(R_pnindex, rhs(x)@data, NULL, FALSE)

  for (r in unique(rr)) {
    pos <- which(rr == r)

    ### FALSE is for verbose
    qsubmax <-
      .Call(R_pnmax, lhs(x[pos])@data, q[pos, "UL"], FALSE)

    imp[pos] <- q[pos, "LL"] - qsubmax
  }

  imp
}
