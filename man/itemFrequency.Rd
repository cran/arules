\name{itemFrequency}
\alias{itemFrequency}
\alias{itemFrequency,itemMatrix-method}
\alias{itemFrequency,tidLists-method}
\title{Getting Frequency/Support for Single Items}
\description{
  Provides the generic function \code{itemFrequency} and S4 methods to get the
  frequency/support for all single items in an objects based on
  \code{\link[arules:itemMatrix]{itemMatrix}}.  For example, it is used to get the
  single item support from an object of class
  \code{\link[arules:transactions-class]{transactions}} without mining.
}
\usage{
itemFrequency(x, ...)

\S4method{itemFrequency}{itemMatrix}(x, type)

\S4method{itemFrequency}{itemMatrix}(x, type)
}
\arguments{
  \item{x}{an object.}
  \item{\dots}{ further arguments are passed on.}
  \item{type}{ a character string specifying 
    if \code{"relative"}  frequency/support or  \code{"absolute"} 
    frequency/support (item counts) is returned. (default:  \code{"relative"}).
  }
}
\value{
  \code{itemFrequency} returns a named numeric vector.  Each element is the
  frequency/support of the corresponding item in object \code{x}. 
  The items appear in the vector in the same
  order as in the binary matrix in \code{x}.
}
\seealso{ 
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}}
}
\examples{
data("Adult")
itemFrequency(Adult, type = "relative")
}
\keyword{models}
