\name{sample}
\alias{sample}
\alias{sample,itemMatrix-method}
\title{Random Samples and Permutations}
\description{
  Provides the generic function \code{sample} and the S4 method to 
  take a  sample of the specified size from the elements of \code{x}
  using either with or without replacement.
   \code{sample} can be used to sample from 
  \code{\link[arules:transactions-class]{transactions}}.
}
\usage{
 sample(x, size, replace = FALSE, prob = NULL)
\S4method{sample}{itemMatrix}(x, size, replace = FALSE, prob = NULL)
}
\arguments{
  \item{x}{ object to be sampled from.}
  \item{size}{ sample size.}
  \item{replace}{ logical; sample with replacement? }
  \item{prob}{  a vector of probability weights. }
}
\value{
  An object of the same class as \code{x}.
}
\seealso{
  \code{\link{itemMatrix-class}}.
}
\examples{
data("Adult_transactions")

## sample with replacement
s <- sample(Adult_transactions, 500, replace = TRUE)
s
}
\keyword{manip}
