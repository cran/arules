\name{inspect}
\alias{inspect}
\alias{inspect,itemMatrix-method}
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
%\S4method{inspect}{itemsets}(x, ...)
%\S4method{inspect}{rules}(x, ...)
%\S4method{inspect}{transactions}(x, ...)
}
\arguments{
  \item{x}{ a set of associations or transactions or an itemMatrix.}
  \item{\dots}{additional arguments (currently unused)}
}
\seealso{
  \code{\link{itemMatrix-class}},
  \code{\link{itemsets-class}},
  \code{\link{rules-class}},
  \code{\link{transactions-class}}
}
\examples{
data("Adult")
rules <- apriori(Adult)
inspect(rules[1000])
}
\keyword{print}
