\name{size}
\alias{size}
\alias{size,itemMatrix-method}
\alias{size,itemsets-method}
\alias{size,rules-method}
\title{Getting the Size of Each Element}
\description{
  Provides the generic function \code{size} and S4 methods to get the
  size of each element from objects based on
  \code{\link[arules:itemMatrix]{itemMatrix}}.
  For example, it is used to get a vector of transaction sizes (i.e., 
  the number of present items (ones) per element (row) of the binary
  incidence matrix) from an object of class
  \code{\link[arules:transactions-class]{transactions}}).
}
\usage{
size(x)
\S4method{size}{itemMatrix}(x)
\S4method{size}{itemsets}(x)
\S4method{size}{rules}(x)
}
\arguments{
  \item{x}{an object.}
}
\value{
  \code{size} returns a numeric vector of length \code{length(x)}.
  Each element is the size of the corresponding element (row in the
  matrix; for rules it is the the sum of the elements in the RHS and the RHS) 
  in object \code{x}.
}
\seealso{ 
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}}
}
\examples{
data("Adult_transactions")
size(Adult_transactions)
}
\keyword{methods}
