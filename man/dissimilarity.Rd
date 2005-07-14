\name{dissimilarity}
\alias{dissimilarity}
\alias{dissimilarity,itemMatrix-method}
\alias{dissimilarity,associations-method}
\title{
  Dissimilarity Computation
}
\description{
  Provides the generic function \code{dissimilarity} and the S4 methods
  to compute and return a distance matrix 
  for  \code{\link[arules:transactions-class]{transactions}} or 
  \code{\link[arules:associations-class]{associations}}
  using 
  several distance measures for binary data.
}
\usage{
dissimilarity(x, y = NULL, method = NULL, args = NULL)
\S4method{dissimilarity}{itemMatrix}(x, y = NULL, method = NULL, args = NULL)
\S4method{dissimilarity}{associations}(x, y = NULL, method = NULL, args = NULL)
}
\arguments{
  \item{x}{ the set of elements (e.g., itemMatrix, transactions, 
    itemsets, rules). }
  \item{y}{ currently unused. }
  \item{method}{ the distance measure to be used. Implemented measures
  are:
   \describe{
       \item{\code{"Jaccard"}:}{ the number of items  which occur in both elements divided by the total number of items in the elements. This measure is often 
       also called: binary, asymmetric binary, etc. }
       \item{\code{"Affinity"}:}{ measure based on 
       the \code{\link{affinity}}},
       a similarity measure between items. It is defined as the
       average \emph{affinity} between the items in two transactions 
       (see references).}
  }
  \item{args}{ a list of additional arguments for the methods.  
  
     For calculating
     \code{"Affinity"} for associations, the affinities between the items in 
     the transactions are needed and passed to the method as the first 
     element in \code{args}.}
}
\value{
  returns an object of class \code{"dist"}.
}
\seealso{
  \code{\link{affinity}},
  \code{\link{itemMatrix-class}},
  \code{\link{associations-class}}
}
\examples{
data(Adult_transactions)

## cluster transactions
## choose a sample 
s <- sample(Adult_transactions, 500) 

##  calculate Jaccard distances and do hclust
d_jaccard <- dissimilarity(s)
plot(hclust(d_jaccard))

## calculate affinity-based distances and do hclust
d_affinity <- dissimilarity(s, method = "Affinity")
plot(hclust(d_affinity))


## cluster rules
## mine rules
rules <- apriori(Adult_transactions)
## choose promising rules
rules <- subset(rules, subset = lift > 2)

## we need to supply the item affinities from the dataset (sample)
d_affinity <- dissimilarity(rules, method = "Affinity", 
  args = list(affinity = affinity(s)))
plot(hclust(d_affinity))
}
\references{
  Charu C. Aggarwal, Cecilia Procopiuc, and Philip S. Yu.
    \emph{Finding localized associations in market basket data.}
  IEEE Trans. on Knowledge and Data Engineering, 14(1):51-62, 2002.
}
\keyword{cluster}
