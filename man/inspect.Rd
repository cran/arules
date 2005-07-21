\name{inspect}
\alias{inspect}
\alias{inspect,itemsets-method}
\alias{inspect,rules-method}
\alias{inspect,transactions-method}
\title{Display Associations and Transactions in Readable Form}
\description{
  Provides the generic function \code{inspect} and S4 methods to display
  associations and transactions plus additional information formatted for
  online inspection.
}
\usage{
inspect(x, ...)
\S4method{inspect}{itemsets}(x, ...)
\S4method{inspect}{rules}(x, ...)
\S4method{inspect}{transactions}(x, ...)
}
\arguments{
  \item{x}{the object.}
  \item{\dots}{additional arguments (currently unused)}
}
\seealso{
  \code{\link{itemsets-class}},
  \code{\link{rules-class}},
  \code{\link{transactions-class}}
}
\examples{
data("Adult_transactions")
rules <- apriori(Adult_transactions)
inspect(rules[1000])
}
\keyword{methods}
