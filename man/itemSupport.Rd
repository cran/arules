\name{itemSupport}
\alias{itemSupport}
\alias{itemSupport,itemMatrix-method}
\alias{itemSupport,tidList-method}
\title{Getting the Support of all Items}
\description{
  Provides the generic function \code{itemSupport} and S4 methods to get the
  support of all items in a objects based on
  \code{\link[itemMatrix]{itemMatrix}}.  For example, it is used to get the
  item support from an object of class
  \code{\link[transactions-class]{transactions}} without mining.
}
\usage{
itemSupport(x, ...)
\S4method{itemSupport}{itemMatrix}(x, type)
\S4method{itemSupport}{itemMatrix}(x, type)
}
\arguments{
  \item{x}{an object.}
  \item{\dots}{ further arguments are passed on.}
  \item{type}{ a character specifying 
    if \code{"relative"}  support or  \code{"absolute"} 
    support (item counts) is returned. (default:  \code{"relative"}).
  }
}
\value{
  \code{itemSupport} returns a named numeric vector.  Each element is the
  itemSupport of the corresponds to an item in object \code{x} with the same
  name. The items in the vector appear in the order of the item appearance in
  the binary matrix.
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
itemSupport(Adult_transactions)
}
\keyword{methods}
