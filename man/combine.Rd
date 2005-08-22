\name{combine}
\alias{combine}
\alias{combine,itemMatrix-method}
\alias{combine,transactions-method}
\alias{combine,rules-method}
\alias{combine,itemsets-method}
\title{Combining Objects}
\description{
  Provides the generic function \code{combine} and the S4 methods to 
  combine several objects based on \code{\link[arules:itemMatrix-class]{itemMatrix}}
  into a single object.
  For example, to combine several mined \code{\link[arules:itemsets-class]{itemsets}} 
  (or \code{\link[arules:rules-class]{rules}}) into a single set.
}
\usage{
combine(first, \dots)
%\S4method{combine}{itemMatrix}(first, ..., as_list)
%\S4method{combine}{transactions}(first, ...)
%\S4method{combine}{rules}(first, ...)
%\S4method{combine}{itemsets}(first, ...)
}
\arguments{
  \item{first}{ first object.}
  \item{\dots}{ further objects to be combined.}
%  \item{as_list}{ used internally.}
}
\value{
  An object of the same class as \code{first}.
}
\seealso{
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}},
  \code{\link{rules-class}},
  \code{\link{itemsets-class}}
}
\examples{
data("Adult_transactions")

### combine transactions
a1 <- Adult_transactions[1:10]
a2 <- Adult_transactions[101:110]

a_comb <- combine(a1, a2)
summary(a_comb)

### combine rules
r1 <- apriori(Adult_transactions[1:1000])
r2 <- apriori(Adult_transactions[1001:2000])
r_comb <- unique(combine(r1, r2)) 
r_comb

}
\keyword{manip}
