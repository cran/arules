\name{LIST}
\alias{LIST}
\alias{LIST,itemMatrix-method}
\alias{LIST,transactions-method}
\alias{LIST,tidLists-method}
\alias{coerce,dgCMatrix,list-method}
\alias{coerce,list,dgCMatrix-method}
\title{
  List Representation for Objects Based on ``itemMatrix''
}
\description{
  Provides the generic function \code{LIST} and the S4 method to create
  a list representation from objects based on
  \code{\link[arules:itemMatrix-class]{itemMatrix}} (e.g.,
  \code{\link[arules:transactions-class]{transactions}},
  \code{\link[arules:tidLists-class]{tidLists}}, or
  \code{\link[arules:itemsets-class]{itemsets}}).
  These methods are also used for the coercion to \code{list}.
}
\usage{
LIST(from, \dots)

\S4method{LIST}{itemMatrix}(from, decode = TRUE)

\S4method{LIST}{transactions}(from, decode = TRUE)

\S4method{LIST}{tidLists}(from, decode = TRUE)
}
\arguments{
  \item{from}{the object to be converted into a list.}
  \item{\dots}{further arguments.}
  \item{decode}{\code{"logical"}; controlling whether the items/transactions are
    decoded from the column numbers internally used by
    \code{\link[arules:itemMatrix-class]{itemMatrix}} to the names stored in
    the object \code{from}.  The default behavior is to decode.
  }
}
\details{
  \code{LIST} returns the object \code{from} as a list of vectors.
  Each vector represents one row of the
  \code{\link[arules:itemMatrix-class]{itemMatrix}} (e.g., items in a
  transaction or itemset).
}
\value{
  a \code{"list"} primitive.
}
\seealso{
  \code{\link{coerce,itemMatrix,list-method}},
  \code{\link{itemMatrix-class}}
}
\examples{
data(Adult)

LIST(Adult[1:5])
LIST(Adult[1:5], decode = FALSE)
}
\keyword{manip}
