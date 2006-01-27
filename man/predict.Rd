\name{predict}
\alias{predict}
\alias{predict,itemMatrix-method}
\title{Model Predictions}
\description{
  Provides the S4 method
  \code{predict} for \code{itemMatrix} (e.g., transactions). 
  Predicts the membership (nearest neighbor) of new data to 
  clusters represented by
  medoids or labeled examples.
  }
\usage{
\S4method{predict}{itemMatrix}(object,   newdata, labels = NULL, \ldots)
}
\arguments{
    \item{object}{ medoids (no labels needed) or examples (labels needed). }
  \item{newdata}{ objects to predict labels for. }
  \item{labels}{ \code{"numeric"}; labels for the examples in \code{object}. }
  \item{\ldots}{ further arguments (currently unused). }
}
\value{
   An integer vector of the same length as \code{newdata}  
   containing the predicted labels for each element.
}
\seealso{
  \code{\link{itemMatrix-class}}
}
\examples{
data("Adult")

### sample
small <- sample(Adult, 500)
large <- sample(Adult, 5000)

### cluster a small sample
d_jaccard <- dissimilarity(small)
hc <- hclust(d_jaccard)
l <-  cutree(hc, k=4)

### predict labels for a larger sample
labels <- predict(small, large, l)


### plot the profile of the 1. cluster
itemFrequencyPlot(large[labels==1, itemFrequency(large) > 0.1])
}
\keyword{models}
\keyword{manip}
