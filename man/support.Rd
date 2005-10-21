\name{support}
\alias{support}
\alias{support,itemMatrix-method}
\alias{support,associations-method}
\title{Support Counting for Itemsets}
\description{
   Provides the generic function and the needed S4 method to
   count support for given itemsets
   (and other types of associations) 
   in a given transaction database. 
}
\usage{
support(x, transactions, ...)
\S4method{support}{itemMatrix}(x, transactions, type= c("relative", "absolute"))
\S4method{support}{associations}(x, transactions, type= c("relative", "absolute"))

}
\arguments{
  \item{x}{ the set of itemsets for which support should be counted. }
  \item{\dots}{ further arguments are passed on. }
  \item{transactions}{ the transaction data set used for mining. }
  \item{type}{ a character string specifying
    if \code{"relative"}  support or  \code{"absolute"}
    support (counts) are returned for the itemsets in \code{x}. 
    (default:  \code{"relative"})}
}
\details{
   Normally, itemset support is counted during mining the database
   with a set minimum support. However, if only the support information 
   for a single or a few itemsets is needed, one might not want to 
   mine the database for all frequent itemsets.
   
   Support is counted using transaction ID list intersection
   which is used by several fast mining algorithms
   (e.g., by Eclat). However, Support is determined for each itemset
   individually which is slow for a large number of long itemsets
   in dense data. In this case mining the whole database might
   be preferable.
}
\value{
   A numeric vector of the same length as \code{x} containing
   the support values for the sets in \code{x}.
}

\seealso{ 
\code{\link{itemMatrix-class}}, 
\code{\link{associations-class}}, 
\code{\link{transactions-class}}
}
\examples{
data("Income")

### find and some frequent itemsets
itemsets <- eclat(Income)[1:5]

### inspect the support returned by eclat
inspect(itemsets)

### count support in the database
support(items(itemsets), Income)
}
\keyword{models}
