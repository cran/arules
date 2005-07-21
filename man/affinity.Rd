\name{affinity}
\alias{affinity}
\alias{affinity,itemMatrix-method}
\title{
  Affinity Computation
}
\description{
  Provides the generic function \code{affinity} and the S4 method
  to compute and return a matrix with the affinities between items  
  for  a set of \code{\link[arules:transactions-class]{transactions}}. 
}
\usage{
affinity(x)
\S4method{affinity}{itemMatrix}(x)
}
\arguments{
  \item{x}{ an object of class \code{"itemMatrix"}.} 
}
\details{
Affinity between the two items \eqn{i} and \eqn{j} is defined as
\deqn{A(i,j) = \frac{sup(\{i,j\})}{sup(\{i\}) + sup(\{j\}) - sup(\{i,j\})},}{A(i,j) = sup(\{i,j\})/(sup(\{i\}) + sup(\{j\}) - sup(\{i,j\})),}
where \eqn{sup(.)} is the support measure. This means that affinity is the 
\emph{Jaccard similarity} between items.
}
\value{
  returns an object of class \code{"matrix"} which represents
  the affinities (similarities) between items in \code{x}.
}
\seealso{
  \code{\link{itemMatrix-class}}
}
\examples{
data(Adult_transactions)

## choose a sample, calculate affinities 
s <- sample(Adult_transactions, 500)
s

a <- affinity(s)
a
}
\references{
   Charu C. Aggarwal, Cecilia Procopiuc, and Philip S. Yu.
    \emph{Finding localized associations in market basket data.}
    IEEE Trans. on Knowledge and Data Engineering, 14(1):51-62, 2002.
}
\keyword{cluster}
