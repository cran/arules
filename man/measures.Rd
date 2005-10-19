\name{measures}
\alias{allConfidence}
\alias{allConfidence,itemsets-method}
\alias{hyperLift}
\alias{hyperLift,rules-method}
\alias{hyperConfidence}
\alias{hyperConfidence,rules-method}
\title{Calculating Additional Interest Measures for Existing Associations}
\description{
   Provides the generic functions and the needed S4 methods to
   calculate some additional interest measures for a set of 
   existing associations.
}
\usage{
allConfidence(x, ...)
\S4method{allConfidence}{itemsets}(x, transactions = NULL, itemSupport = NULL)

hyperLift(x, ...)
\S4method{hyperLift}{rules}(x, transactions, d = 0.99)

hyperConfidence(x, ...)
\S4method{hyperConfidence}{rules}(x, transactions = NULL,  
	complements = TRUE, significance = FALSE)
}
\arguments{
  \item{x}{ the set of associations. }
  \item{\dots}{ further arguments. }
  \item{transactions}{ the transaction data set used to mine 
  	the associations. }
  \item{itemSupport}{ alternatively to transactions, for some measures 
  	a item support in the transaction data set is sufficient.}
  \item{d}{ the quantile used to calculate hyperlift. }  
  \item{complements}{ calculate convidence/significance levels for
   substitutes instead of complements. }  
  \item{significance}{ report significance levels instead of confidence levels. }  
}
\details{
  Currently the following interest measures are implemented:
  
  \describe{
  \item{All-confidence (see, Omiencinski, 2003)}{ 
  is defined on itemsets as the minimum confidence of
  all possible rule
  generated from the itemset.
  }
  
  \item{Hyper-lift (see, Hahsler et al., 2005)}{
  is an adaptation of the lift 
  measure which is more robust 
  for low counts. It is based on the idea that under independence 
  the count \eqn{c_{XY}} of the transactions which contain 
  all items in a rule
  \eqn{X \Rightarrow Y}{X -> Y} follows a hypergeometric distribution 
  (represented by the random variable \eqn{C_{XY}}) with
  the parameters given by the counts  \eqn{c_X} and  \eqn{c_Y}.
  
  Lift is defined for the rule \eqn{X \Rightarrow Y}{X -> Y} 
  as:
  \deqn{\mathrm{lift}(X \Rightarrow Y) = \frac{P(X \cup Y)}{P(X)P(Y)} 
  = \frac{c_{XY}}{E[C_{XY}]},}{lift(X -> Y) = P(X+Y)/(P(X)*P(Y)) = c_XY / E[C_XY],}
  
  where \eqn{E[C_{XY}] = c_X c_Y / m} with \eqn{m} being the number
  of transactions in the datanase.

  Hyper-lift is defined as:
  \deqn{\mathrm{hyperlift}(X \Rightarrow Y) = \frac{c_{XY}}{Q_{\delta}[C_{XY}]},}{hyperlift(X -> Y) = c_XY / Q_d[C_XY],}

  where  \eqn{Q_{\delta}[C_{XY}]}{Q_d[C_XY]} is the
  quantile of the hypergeometric distribution given by \eqn{\delta}{d}.
  }
  \item{Hyper-confidence (based on Hahsler et al., 2005)}{
  calculates the confidence level that we observe too high/low counts 
  for rules \eqn{X \Rightarrow Y}{X -> Y} using the hypergeometric model.
  Since the counts are drawn from a hypergeometric distribution 
  (represented by the random variable \eqn{C_{XY}}) with
  known parameters given by the counts  \eqn{c_X} and  \eqn{c_Y},
  we can calculate a confidence interval for the observed counts 
  \eqn{c_{XY}} steming from the distribution. Hyperconfidence
  reports the confidence level (significance level) for
  \describe{
  \item{complements -}{
    \eqn{1 - P[C_{XY} >= c_{XY} | c_X, c_Y]}
  }
  \item{substitutes -}{
    \eqn{1 - P[C_{XY} < c_{XY} | c_X, c_Y]}.
  }
  }
  A confidence level of, e.g., \eqn{> 0.95} indicates that
  there is only a  5\% chance that the count for the rule was generated
  randomly.
 
  
  }
}
}
\value{
   A numeriv vector containing the values of the interest measure 
   for each association
   in the set of associations \code{x}.
}
\references{
 Edward R. Omiecinski. Alternative interest measures for mining 
  associations in databases. IEEE Transactions on Knowledge and 
  Data Engineering, 15(1):57-69, Jan/Feb 2003.
 
 Michael Hahsler, Kurt Hornik, and Thomas Reutterer. 
  Implications of probabilistic data modeling for rule mining. 
  Report 14, Research Report Series, Department of Statistics and 
  Mathematics, Wirschaftsuniversität Wien, Augasse 2-6, 1090 Wien, 
  Austria, March 2005.
}

\seealso{ \code{\link{itemsets-class}}, \code{\link{rules-class}} }
\examples{
data("Income")

### calculate all-confidence
itemsets <- apriori(Income, parameter = list(target = "freq")) 
quality(itemsets) <- cbind(quality(itemsets), 
	allConfonfidence = allConfidence(itemsets))

summary(itemsets)

### calculate hyperlift for the 0.9 quantile
rules <- apriori(Income)
quality(rules) <- cbind(quality(rules), 
	hyperLift = hyperLift(rules, Income, d = 0.9))

inspect(SORT(rules, by = "hyperLift")[1:5])

### calculate hyper-confidence and discard all rules with
### a confidence level < 1\%
quality(rules) <- cbind(quality(rules),
 	hyperConfidence = hyperConfidence(rules, Income))

rulesHConf <- rules[quality(rules)$hyperConfidence >= 0.99]

inspect(rulesHConf[1:10])

}
\keyword{models}
