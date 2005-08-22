\name{random.transactions}
\alias{random.transactions}
\title{Create a Random Transaction Data Set}
\description{
  Creates a random
  \code{\link[arules:transactions-class]{transactions}} object 
  where all items occur independently of each other.
}
\usage{
random.transactions(nitems, ntrans, prob = 0.1)
}
\arguments{
  \item{nitems}{\code{"integer"}; number of items.}
  \item{ntrans}{\code{"integer"}; number of transactions.}
  \item{prob}{ \code{"numeric"}; occurrence probabilities for items in 
	the transactions. If \code{prob} is a vector of length \code{nitems}, 
	each value is interpreted as the
     	occurrence probability of the corresponding item. If \code{prob} is
	a scalar, the same given probability is used for all items. }
}
\details{
  The function generates a \code{nitems} times \code{ntrans} 
  transaction database.
  All items are treated as independent. 
  Each transaction is the result of \code{nitems} independent Bernoulli trials,   one for each item with success probabilities given by \code{prob}.
}
\value{
  Returns an object of class
  \code{\link[arules:transactions-class]{transactions}}.
}
\seealso{
  \code{\link{transactions-class}}.
}
\references{
Michael Hahsler, Kurt Hornik, and Thomas Reutterer. Implications 
of probabilistic data modeling for mining association rules. 
In Proceedings of the 29th Annual Conference of the Gesellschaft 
fuer Klassifikation e.V., University of Magdeburg, March 9-11, 2005, 
Studies in Classification, Data Analysis, and Knowledge Organization. 
Springer-Verlag, 2005. Accepted for publication.
}
\examples{
 # generate random 1000 transactions for 200 items with 
 # a success probability decreasing from 0.2 to 0.0001
 trans <- random.transactions(200, 1000, seq(0.2,0.0001, length=200))
 
 # display random data set
 image(trans)
}
\keyword{datagen}
