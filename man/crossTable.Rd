\name{crossTable}
\alias{crossTable}
\alias{crossTable,itemMatrix-method}
\title{Cross-tabulate joint occurrences across pairs of items}
\description{
  Provides the generic function \code{crossTable} and the S4 method to 
  cross-tabulate joint occurrences across pairs of items.
}
\usage{
crossTable(x, \ldots)
}
\arguments{
  \item{x}{ object to be cross-tabulated 
    (\code{transactions} or \code{itemMatrix}).}
  \item{\ldots}{ further arguments (currently unused). }
}
\value{
  A symmetric matrix of n time n, where n is the number of items times 
  in \code{x}. The matrix contains the co-occurrence counts between pairs of
  items.
}
\seealso{
  \code{\link{transactions-class}},
  \code{\link{itemMatrix-class}}.
}
\examples{
data("Groceries")

ct <- crossTable(Groceries)
ct[1:5, 1:5]

}
\keyword{models}
