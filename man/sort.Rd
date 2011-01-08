\name{sort}
\alias{sort}
\alias{SORT}
\alias{SORT,associations-method}
\alias{sort,associations-method}
\title{Sorting Associations}
\description{
  Provides the method \code{sort} to sort elements
  in class \code{\linkS4class{associations}} (e.g., itemsets or rules)
  according to the value of measures stored in the association's slot
  \code{quality} (e.g., support).
}
\usage{
\S4method{sort}{associations}(x, decreasing = TRUE, na.last = NA, 
    by = "support", ...)

## deprecated since arules version 0.5-1
\S4method{SORT}{associations}(x, by = "support", na.last = NA, 
    decreasing = TRUE)
}
\arguments{
  \item{x}{ an object to be sorted.}
  \item{decreasing}{a logical. Should the sort be increasing or
    decreasing? (default is decreasing)}
  \item{na.last}{for controlling the treatment of \code{NAs}. If \code{TRUE},
    missing values in the data are put last; if \code{FALSE}, they are put
    first; if \code{NA}, they are removed.}
  \item{by}{ a character string specifying the quality measure stored in
    \code{x} to be used to sort \code{x}.}
  \item{...}{Further arguments passed on to \code{order}.}
}
\value{
  An object of the same class as \code{x}.
}

\seealso{
  \code{\link{associations-class}}
}
\examples{
data("Adult")

## Mine frequent itemsets with Eclat.
fsets <- eclat(Adult, parameter = list(supp = 0.5))

## Print the 5 itemsets with the highest support as a data.frame.
as(sort(fsets)[1:5], "data.frame")
}
\keyword{manip}
\keyword{arith}
