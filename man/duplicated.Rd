\name{duplicated}
\alias{duplicated}
\alias{duplicated,itemsets-method}
\alias{duplicated,rules-method}
\alias{duplicated,itemMatrix-method}
\title{Find Duplicated Associations and Transactions}
\description{
  Provides the generic function \code{duplicated}  
  and the S4 methods for associations. \code{duplicated} finds 
  duplicated associations or transactions (e.g., identical 
  \code{\link[arules:itemsets-class]{itemsets}},
  \code{\link[arules:rules-class]{rules}} or  
  \code{\link[arules:transactions-class]{transactions}})
  and returns a logical vector indicating which associations
  are duplicates. 
}
\usage{
duplicated(x, incomparables = FALSE, ...)
%\S4method{duplicated}{rules}(x, incomparables = FALSE, ...)
%\S4method{duplicated}{itemsets}(x, incomparables = FALSE, ...)
%\S4method{duplicated}{itemMatrix}(x, incomparables = FALSE, ...)
}
\arguments{
  \item{x}{ a set of associations or transactions. }
  \item{\dots}{ further arguments (currently unused). }
  \item{incomparables}{ argument currently unused.}
}
\value{
   A logical vector indicating duplicated elements.
}
\seealso{
  \code{\link{unique}},
  \code{\link{rules-class}},
  \code{\link{itemsets-class}},
  \code{\link{itemMatrix-class}}
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
