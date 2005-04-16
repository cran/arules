\name{size}
\alias{size}
\alias{size,itemMatrix-method}
\title{Getting the Size of All Elements}
\description{
  Provides the generic function \code{size} and S4 methods to get the
  size of all elements from objects based on
  \code{\link[itemMatrix]{itemMatrix}}.
  For example, it is used to get a vector of transaction sizes (i.e., 
  the number of present items (ones) per element (row) of the binary
  incidence matrix) from an object of class
  \code{\link[transactions-class]{transactions}}).
}
\usage{
size(x)
\S4method{size}{itemMatrix}(x)
}
\arguments{
  \item{x}{an object.}
}
\value{
  \code{size} returns a numeric vector of length \code{length(x)}.
  Each element is the size of the corresponding element (row in the
  matrix) in object \code{x}.
}
\author{
  Michael Hahsler
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
