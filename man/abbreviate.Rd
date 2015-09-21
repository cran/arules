\name{abbreviate}
\Rdversion{1.1}
\alias{abbreviate}
\alias{abbreviate.transactions}
\alias{abbreviate.rules}
\alias{abbreviate.itemMatrix}
\title{Abbreviate function for item labels in transactions, 
itemMatrix and rules}
\description{
Methods to abbreviate long item labels in transactions and rules. 
}
\usage{
\method{abbreviate}{transactions}(data, minlength = 4, ...,method = "both.sides")
\method{abbreviate}{rules}(data, minlength = 4, ...,method = "both.sides")
\method{abbreviate}{itemMatrix}(data, minlength = 4, ...,method = "both.sides")
}
\arguments{
  \item{data}{ an object of class "transactions", "itemMatrix" and "rules"}
  \item{minlength}{ characters allwed in abbreviation}
  \item{method}{ apply to level and value (both.sides) }
  \item{\dots}{ further arguments; currently unused }
}
                            
\author{
Sudheer Chelluboina and Michael Hahsler based on code by Martin Vodenicharov.
}
\examples{
data(Adult)
inspect(head(Adult, 1))

Adult_abbr <- abbreviate(Adult, 15)
inspect(head(Adult_abbr, 1))
}
\keyword{ manip }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
