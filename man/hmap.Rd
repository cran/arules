\name{hmap}
\alias{hmap}
\alias{hmap,itemMatrix-method}
\title{
  Draw a Heat Map for Objects Based on ``itemMatrix'' 
}
\description{
  Provides the generic function \code{hmap} and the S4 method
  to draw a heat map (an image with dendrogram added and 
  reordered rows and columns) for objects based on 
  \code{\link[arules:itemMatrix-class]{itemMatrix}} (e.g., transactions).
}
\usage{
hmap(x, \dots)
\S4method{hmap}{itemMatrix}(x, distfun = function(z) dist(z, method = "binary"),
    margins = c(10, 10),
        xlab = "Items (Columns)", ylab = "Elements (Rows)",
	    col = gray(seq(from = 1, to = 0, length = 100)), \dots)
}
\arguments{
  \item{x}{ an object of class \code{"itemMatrix"}.} 
  \item{distfun}{ distance function; default is Jaccard (binary). }
  \item{margins}{ a numeric vector of length 2 containing the margins. }
  \item{xlab, ylab}{ x- and y-axis titles. }
  \item{col}{ used colors. }
  \item{\dots}{ further arguments passed on to \code{\link{heatmap}}.} 
}
\value{
  Invisibly, a list of components from 
  \code{\link{heatmap}}.
}
\details{
\code{hmap} is an interface to \code{\link{heatmap}} for binary transactions. 
In addition
to make Jaccard the standard distance measure, also columns
for items which do not occur in \code{x} are discarded before
calculating distances. This can make computing the heat map significantly 
faster for random samples from a set of transactions.
}
\seealso{
  \code{\link{itemMatrix-class}}
}
\examples{
data(Adult_transactions)

## choose a sample and produce a heat map 
s <- sample(Adult_transactions, 100)
hmap(s)
}
\keyword{hplot}
\keyword{cluster}
