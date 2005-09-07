\name{image}
\alias{image}
\alias{image,itemMatrix-method}
\alias{image,transactions-method}
\alias{image,tidLists-method}
\title{Visual Inspection of Binary Incidence Matrices}
\description{
  Provides the S4 methods \code{image} to
  generate level plots to
  visually inspect 
  binary incidence matrices, i.e.,
  objects based on \code{\link[arules:itemMatrix-class]{itemMatrix}}
  (e.g., transactions, tidLists, items in itemsets or rhs/lhs in rules).
  These plots can be used to identify problems in a data set 
  (e.g., recording problems with some transactions containing all items).
}
\usage{
%image(x, \dots)
\S4method{image}{itemMatrix}(x, ...)

\S4method{image}{transactions}(x, ...)

\S4method{image}{tidLists}(x, ...)
}
\arguments{
  \item{x}{ the object (\code{itemMatrix}, \code{transactions} or 
	\code{tidLists}). }
  \item{\dots}{ further arguments passed on to \code{image} in 
	package \pkg{Matrix}.}
}
\seealso{
  \code{\link[Matrix]{image}} (in \pkg{Matrix}),
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}},
  \code{\link{tidLists-class}}
}
\examples{
data("Epub")

# in this data set we can see that not all
# items were available from the beginning.

image(Epub)

}
\keyword{hplot}
