\name{unique}
\alias{unique}
\alias{unique,associations-method}
\alias{unique,itemMatrix-method}
\title{Remove Duplicated Associations and Transactions}
\description{
  Provides the generic function \code{unique}   
  and the S4 methods for associations and transactions. 
  \code{unique} uses 
  \code{\link{duplicated}}
  to return a set of \code{\link[arules:associations-class]{associations}}
  or  \code{\link[arules:transactions-class]{transactions}}
  with the duplicate elements removed.
}
\usage{
unique(x, incomparables = FALSE, ...)
%\S4method{unique}{associations}(x, incomparables = FALSE, ...)
%\S4method{unique}{itemMatrix}(x, incomparables = FALSE, ...)
}
\arguments{
  \item{x}{ a set of associations. }
  \item{\dots}{ further arguments (currently unused). }
  \item{incomparables}{ currently unused.}
}
\value{
  An object of the same class as \code{x} with duplicated elements removed.
}
\seealso{
  \code{\link{duplicated}},
  \code{\link{associations-class}},
  \code{\link{itemMatrix-class}}
}
\examples{
data("Adult_transactions")

r1 <- apriori(Adult_transactions[1:1000])
r2 <- apriori(Adult_transactions[1001:2000])
r_comb <- combine(r1, r2)
r_comb <- unique(r_comb)
r_comb

}
\keyword{manip}
