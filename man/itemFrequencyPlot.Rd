\name{itemFrequencyPlot}
\alias{itemFrequencyPlot}
\alias{itemFrequencyPlot,itemMatrix-method}
\title{
  Plotting the Item Frequencies for Objects Based on ``itemMatrix''
}
\description{
  Provides the generic function \code{itemFrequencyPlot} and the S4 method 
  to create an item frequency bar plot for inspecting 
  the item frequency distribution for objects based on
  \code{\link[itemMatrix-class]{itemMatrix}} (e.g.,
  \code{\link[transactions-class]{transactions}},
  or items in
  \code{\link[itemsets-class]{itemsets}}
  and 
  \code{\link[rules-class]{rules}}).
}
\usage{
itemFrequencyPlot(x, \dots)
\S4method{itemFrequencyPlot}{itemMatrix}(x, type = "relative", cex.names =  par("cex.axis"), \dots)
}
\arguments{
  \item{x}{the object to be plotted.}
  \item{\dots}{further arguments are passed on (see  
  	\code{\link[barplot]{barplot}} from possible arguments).}
  \item{type}{ indicates whether item frequencies should be displayed 
  relative of absolute.}
  \item{cex.names}{expansion factor for axis names (bar labels).}
  }
}
\value{
A numeric vector with the midpoints of the drawn bars; useful for
adding to the graph.
}
\author{
  Michael Hahsler
}
\seealso{
  \code{\link{itemMatrix-class}}
}
\examples{
data(Adult_transactions)

# the following example compares the item frequencies
# of people with a rage income (boxes) with the average in the data set

# box plot for young people (first 72 items)
midpoints <- itemFrequencyPlot(
  Adult_transactions[Adult_transactions \%in\% "income = large", 1:72])

# add the item frequency over all transactions (first 72 items)
lines(midpoints, itemSupport(Adult_transactions[, 1:72]))


}
\keyword{hplot}
