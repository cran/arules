\name{match}
\alias{match}
\alias{match,itemsets,itemsets-method}
\alias{match,rules,rules-method}
\alias{match,itemMatrix,itemMatrix-method}
\title{Value Matching}
\description{
  Provides the generic function \code{match}  
  and the S4 methods for associations and transactions. 
  \code{match} returns a vector of the positions of (first) matches of
       its first argument in its second.
}
\usage{
match(x,  table, nomatch = NA, incomparables = FALSE)
%\S4method{match}{rules}(x,  table, nomatch = NA, incomparables = FALSE)
%\S4method{match}{itemsets}(x,  table, nomatch = NA, incomparables = FALSE)
%\S4method{match}{itemMatrix}(x,  table, nomatch = NA, incomparables = FALSE)
}
\arguments{
    \item{x}{ an object of class \code{itemMatrix}, \code{transactions} or \code{associations}.}
  \item{table}{ a set of associations or transactions to be matched against. }
  \item{nomatch}{ the value to be returned in the case when no match is found. }
  \item{incomparables}{ not implemented. }
}
\value{
   An integer vector of the same length as \code{x}  giving the position in 
   \code{table} of the
        first match if there is a match, otherwise \code{nomatch}.
}
\seealso{
  \code{\link{rules-class}},
  \code{\link{itemsets-class}},
  \code{\link{itemMatrix-class}}
}
\examples{
data("Adult")

### get unique transactions
vals <- unique(Adult)

### count frequency of unique transactions
cnts <- tabulate(match(Adult, vals))

### plot frequency of unique transactions
plot(sort(cnts, decreasing=TRUE))

}
\keyword{manip}
