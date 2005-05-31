\name{measures}
\alias{all_confidence}
\alias{all_confidence,itemsets-method}
\alias{hyperlift}
\alias{hyperlift,rules-method}
\title{Calculating additional Interest Measures for existing Associations}
\description{
   Provides the generic functions and the needed S4 methods to
   calculate some additional interest measures for a set of 
   existing associations.
}
\usage{
all_confidence(x, ...)
\S4method{all_confidence}{itemsets}(x, transactions = NULL, itemSupport = NULL)
hyperlift(x, ...)
\S4method{hyperlift}{rules}(x, transactions, d = 0.99)
}
\arguments{
  \item{x}{ the set of associations. }
  \item{\dots}{ further arguments are passed on. }
  \item{transactions}{ the transaction data set used to mine 
  	the associations. }
  \item{itemSupport}{ alternatively to transactions, for some measures 
  	a item support in the transaction data set is sufficient.}
  \item{d}{ the quantile used to calculate hyperlift. }  
}
\details{
  Currently the interest measures all-confidence and hyperlift are implemented.
  
  All-confidence is defined on itemsets as the minimum confidence of
  all possible rule
  generated from the itemset.

  Hyperlift is an adaptation of the lift measure which is more robust 
  for low counts.
  
}
\value{
   A vector containing the values of the interest measure for each association
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
\author{ Michael Hahsler }

\seealso{ \code{\link{itemsets-class}}, \code{\link{rules-class}} }
\examples{
data("Income_transactions")

### calculate all-confidence
itemsets <- apriori(Income_transactions, parameter = list(target = "freq")) 
quality(itemsets) <- cbind(quality(itemsets), 
	all_confonfidence = all_confidence(itemsets))
summary(itemsets)

### calculate hyperlift for the 0.9 quantile
rules <- apriori(Income_transactions)
quality(rules) <- cbind(quality(rules), 
	hyperlift = hyperlift(rules, Income_transactions, d = 0.9))
inspect(SORT(rules, by = "hyperlift")[1:5])

}
\keyword{methods}
\keyword{models}
