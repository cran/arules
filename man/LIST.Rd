\name{LIST}
\alias{LIST}
\alias{LIST,itemMatrix-method}
\alias{LIST,transactions-method}
\alias{LIST,tidList-method}
\title{
  Creating a List Representation from Objects Based on ``itemMatrix''
}
\description{
  Provides the generic function \code{LIST} and the S4 method to create
  a list representation from objects based on
  \code{\link[itemMatrix-class]{itemMatrix}} (e.g.,
  \code{\link[transactions-class]{transactions}},
  \code{\link[tidList-class]{tidList}}, or
  \code{\link[itemsets-class]{itemsets}}).
  These methods are also used for the coercion to \code{list}.
}
\usage{
LIST(from, \dots)
\S4method{LIST}{itemMatrix}(from, decode = TRUE)
\S4method{LIST}{transactions}(from, decode = TRUE)
\S4method{LIST}{tidList}(from, decode = TRUE)
}
\arguments{
  \item{from}{the object to be converted into a list.}
  \item{\dots}{further arguments are passed on.}
  \item{decode}{a logical controlling whether the items/transactions are
    decoded from the column numbers internally used by
    \code{\link[itemMatrix-class]{itemMatrix}} to the names stored in
    the object \code{from}.  The default behavior is to decode.
  }
}
\details{
  \code{LIST} returns the object \code{from} as a list of vectors.
  Each vector represents one row of the
  \code{\link[itemMatrix-class]{itemMatrix}} (e.g., items in a
  transaction or itemset).
}
\value{
  a \code{"list"} primitive.
}
\author{
  Michael Hahsler
}
\seealso{
  \code{\link{itemMatrix-class}}
}
\examples{
data(Adult_transactions)

LIST(Adult_transactions[1:5])
LIST(Adult_transactions[1:5], decode = FALSE)
}
\keyword{methods}
