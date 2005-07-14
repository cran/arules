\name{SORT}
\alias{SORT}
\alias{SORT,associations-method}
\title{Sorting Associations}
\description{
  Provides the generic function \code{SORT} and the S4 method to sort 
  elements in class \code{\link[arules:associations-class]{associations}}
  (e.g., itemsets or rules).
}
\usage{
SORT(x, \dots)
\S4method{SORT}{associations}(x, by = "support", decreasing = TRUE)
}
\arguments{
  \item{x}{ an object to be sorted.}
  \item{\dots}{ further arguments are passed on.}
  \item{by}{ a character specifying the quality measure stored in
    \code{x} to be used to sort \code{x}.}
  \item{decreasing}{logical.  Should the sort be increasing or
    decreasing?}
}
\value{
  An object of the same class as \code{x}.
}

\seealso{
  \code{\link{associations-class}}
}
\examples{
data("Adult_transactions")

## Mine frequent itemsets with Eclat.
fsets <- eclat(Adult_transactions, parameter = list(supp = 0.5))

## Print the 5 itemsets with the highest support as a list.
as(SORT(fsets)[1:5], "list")
}
\keyword{manip}
