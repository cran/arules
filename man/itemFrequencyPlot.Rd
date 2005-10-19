\name{itemFrequencyPlot}
\alias{itemFrequencyPlot}
\alias{itemFrequencyPlot,itemMatrix-method}
\title{
  Creating a Item Frequencies/Support Bar Plot
}
\description{
  Provides the generic function \code{itemFrequencyPlot} and the S4 method 
  to create an item frequency bar plot for inspecting 
  the item frequency distribution for objects based on
  \code{\link[arules:itemMatrix-class]{itemMatrix}} (e.g.,
  \code{\link[arules:transactions-class]{transactions}},
  or items in
  \code{\link[arules:itemsets-class]{itemsets}}
  and 
  \code{\link[arules:rules-class]{rules}}).
}
\usage{
itemFrequencyPlot(x, \dots)
\S4method{itemFrequencyPlot}{itemMatrix}(x, type = "relative", 
    population = NULL, deviation = FALSE, horiz = FALSE, 
    cex.names =  par("cex.axis"), xlab = NULL, ylab = NULL, 
    mai = NULL, \dots)
}
\arguments{
  \item{x}{the object to be plotted.}
  \item{\dots}{further arguments are passed on (see  
  	\code{\link[graphics]{barplot}} from possible arguments).}
  \item{type}{ a character string indicating whether 
	item frequencies should be displayed 
  relative of absolute.}
  \item{population}{ object of same class  as \code{x}; if \code{x}
    is a segment of a population, the population mean frequency for
    each item can be shown as a line in the plot.}
  \item{deviation}{\code{"logical"}; plot the relative deviation from the
    population mean instead of frequencies.}
  \item{horiz}{\code{"logical"}; If \code{FALSE} (default), 
    the bars are drawn vertically. If \code{TRUE}, the bars are
    drawn horizontally.}
  \item{cex.names}{\code{"numeric"}; expansion factor for axis names (bar labels).}
  \item{xlab}{a character string with the label for the x axis (use an empty string to force no label).}
  \item{ylab}{a character string with the label for the y axis (see xlab).}
  \item{mai}{a numerical vector giving the plots margin sizes in inches (see `? par').}
  }
}
\value{
A numeric vector with the midpoints of the drawn bars; useful for
adding to the graph.
}
\seealso{
  \code{\link{itemMatrix-class}}
}
\examples{
data(Adult)

# the following example compares the item frequencies
# of people with a large income (boxes) with the average in the data set
Adult.largeIncome <- Adult[Adult \%in\% 
	"income=large"]

# simple plot
itemFrequencyPlot(Adult.largeIncome)

# plot with the averages of the population plotted as a line 
# (first 72 variables/items)
itemFrequencyPlot(Adult.largeIncome[, 1:72], 
	population = Adult[, 1:72])

# plot of deviations from population mean (horizontal bars)
itemFrequencyPlot(Adult.largeIncome[, 1:72], 
        population = Adult[, 1:72], 
	deviation = TRUE, horiz = TRUE)
}
\keyword{hplot}
