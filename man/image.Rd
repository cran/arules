\name{image}
\alias{image}
\alias{image,itemMatrix-method}
\alias{image,transactions-method}
\title{Visual Inspection of an Object Based on ``itemMatrix''}
\description{
  Provides the generic function \code{image} and the S4 methods to
  generate level plots to
  visually inspect objects based on \code{\link[arules:itemMatrix-class]{itemMatrix}}
  (e.g., transactions, items in itemsets or rhs/lhs in rules).
  These plots can be used to identify problems in a data set 
  (e.g., recording problems with some transactions containing all items).
}
\usage{
image(x, \dots)
\S4method{image}{itemMatrix}(x, ...)
\S4method{image}{transactions}(x, ...)
}
\arguments{
  \item{x}{ the object. }
  \item{\dots}{ further arguments passed on.}
}
\seealso{
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}}
}
\examples{
data("Epub")

# in this data set we can see that not all
# items were available from the beginning.

image(Epub)

}
\keyword{hplot}
