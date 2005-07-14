\name{duplicated}
\alias{duplicated}
\alias{duplicated,itemsets-method}
\alias{duplicated,rules-method}
\title{Find Duplicated Associations}
\description{
  Provides the generic function \code{duplicated}  
  and the S4 methods for associations. \code{duplicated} finds 
  duplicated associations (e.g., identical 
  \code{\link[arules:itemsets-class]{itemsets}}
  or \code{\link[arules:rules-class]{rules}})
  and returns a logical vector indicating which associations
  are duplicates. 
}
\usage{
duplicated(x, incomparables = FALSE, ...)
\S4method{duplicated}{rules}(x, incomparables = FALSE, ...)
\S4method{duplicated}{itemsets}(x, incomparables = FALSE, ...)
}
\arguments{
  \item{x}{ a set of associations. }
  \item{\dots}{ further arguments passed on. }
  \item{incomparables}{ argument passed on.}
}
\value{
   A logical vector.
}
\seealso{
  \code{\link{unique}},
  \code{\link{rules-class}},
  \code{\link{itemsets-class}}
}
\examples{
data("Adult_transactions")

r1 <- apriori(Adult_transactions[1:1000])
r2 <- apriori(Adult_transactions[1001:2000])
r_comb <- combine(r1, r2)
duplicated(r_comb)
r_comb

}
\keyword{manip}
