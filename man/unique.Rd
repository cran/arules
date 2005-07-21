\name{unique}
\alias{unique}
\alias{unique,associations-method}
\title{Remove Duplicated Associations}
\description{
  Provides the generic function \code{unique}   
  and the S4 methods for associations. 
  \code{unique} uses 
  \code{\link{duplicated}}
  to return a set of \code{\link[arules:associations-class]{associations}}
  with the duplicate associations removed.
}
\usage{
unique(x, incomparables = FALSE, ...)
\S4method{unique}{associations}(x, incomparables = FALSE, ...)
}
\arguments{
  \item{x}{ a set of associations. }
  \item{\dots}{ further arguments passed on. }
  \item{incomparables}{ argument passed on.}
}
\value{
  An object of the same class as \code{x}.
}
\seealso{
  \code{\link{duplicated}},
  \code{\link{associations-class}}
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
