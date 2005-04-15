\name{length}
\alias{length,itemMatrix-method}
\alias{length,rules-method}
\alias{length,itemsets-method}
\title{Getting the Number of Elements}
\description{
  S4 methods for \code{length} which return the number of elements of
  objects defined in the package \pkg{arules}.
}
\details{
  For \code{\link[itemMatrix-class]{itemMatrix}} and
  \code{\link[transactions-class]{transactions}} the length is defined
  as the number of rows (transactions) in the binary incidence matrix.
  
  For sets of associations (\code{\link[rules-class]{rules}},
  \code{\link[itemsets-class]{itemsets}} and
  \code{\link[associations-class]{associations}} in general) the length
  is defined as the number of elements in the set (i.e., the number of
  rules or itemsets).
  
  For \code{\link[tidList-class]{tidList}} the length is the number of
  elements in the object (a transaction ID list belongs to a set of
  itemsets and contains for each itemset in the set one list element
  containing several transaction IDs).
}
\usage{
%\S4method{length}{transactions}(x)
\S4method{length}{rules}(x)
\S4method{length}{itemsets}(x)
%\S4method{length}{tidList}(x)
\S4method{length}{itemMatrix}(x)
}
\arguments{
  \item{x}{ an object of class 
  \code{\link[transactions-class]{transactions}},
  \code{\link[rules-class]{rules}},
  \code{\link[itemsets-class]{itemsets}},
  \code{\link[tidList-class]{tidList}}, or
  \code{\link[itemMatrix-class]{itemMatrix}}.
  }
  }

\value{
  An \code{integer} giving the \dQuote{length} of \code{x}.
}
\author{
  Michael Hahsler
}
\keyword{methods}
