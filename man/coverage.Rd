\name{coverage}
\alias{coverage}
\alias{coverage,rules-method}
\title{Calculate coerage for rules}
\description{
   Provides the generic function and the needed S4 method to
   calculate the coverage (support of the left-hand-side) of rules.
   }
\usage{
coverage(x)
}
\arguments{
  \item{x}{ the set of rules. }
}
\details{
  Coverage is calculated from the rules quality measures 
  (support and confidence) stored in the quality slot. 
}
\value{
   A numeric vector of the same length as \code{x} containing
   the coverage values for the sets in \code{x}.
}

\seealso{ 
\code{\link{rules-class}} 
}
\examples{
data("Income")

### find and some rules
rules <- apriori(Income)[1:5]

### calculate coverage
quality(rules) <- cbind(quality(rules), coverage = coverage(rules))

inspect(rules)
}
\keyword{models}
