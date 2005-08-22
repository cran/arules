\name{length}
\alias{length,itemMatrix-method}
\alias{length,rules-method}
\alias{length,itemsets-method}
\alias{length,tidLists-method}
\title{Getting the Number of Elements}
\description{
  S4 methods for \code{length} which return the number of elements of
  objects defined in the package \pkg{arules}.
}
\usage{
\S4method{length}{rules}(x)

\S4method{length}{itemsets}(x)

\S4method{length}{tidLists}(x)

\S4method{length}{itemMatrix}(x)
}
\arguments{
  \item{x}{ an object of class 
  \code{\link[arules:transactions-class]{transactions}},
  \code{\link[arules:rules-class]{rules}},
  \code{\link[arules:itemsets-class]{itemsets}},
  \code{\link[arules:tidLists-class]{tidLists}}, or
  \code{\link[arules:itemMatrix-class]{itemMatrix}}.
  }
  }
\details{
  For \code{\link[arules:itemMatrix-class]{itemMatrix}} and
  \code{\link[arules:transactions-class]{transactions}} the length is defined
  as the number of rows (transactions) in the binary incidence matrix.
  
  For sets of associations (\code{\link[arules:rules-class]{rules}},
  \code{\link[arules:itemsets-class]{itemsets}} and
  \code{\link[arules:associations-class]{associations}} in general) the length
  is defined as the number of elements in the set (i.e., the number of
  rules or itemsets).
  
  For \code{\link[arules:tidLists-class]{tidLists}} the length is the number of
  lists (one per item or itemset) in the object.
}
\value{
  An \code{integer} giving the \dQuote{length} of \code{x}.
}
\keyword{attribute}
