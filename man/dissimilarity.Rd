\name{dissimilarity}
\alias{dissimilarity}
\alias{dissimilarity,matrix-method}
\alias{dissimilarity,itemMatrix-method}
\alias{dissimilarity,associations-method}
\title{
  Dissimilarity Computation
}
\description{
  Provides the generic function \code{dissimilarity} and the S4 methods to
  compute and returns distances for binary data in a \code{matrix},
  \code{\linkS4class{transactions}} or \code{\linkS4class{associations}}.
}
\usage{
dissimilarity(x, y = NULL, method = NULL, args = NULL, \ldots)
\S4method{dissimilarity}{itemMatrix}(x, y = NULL, method = NULL, args = NULL,
	which = "transactions")
\S4method{dissimilarity}{associations}(x, y = NULL, method = NULL, args = NULL,
	which = "transactions")
\S4method{dissimilarity}{matrix}(x, y = NULL, method = NULL, args = NULL)
}
\arguments{
  \item{x}{ the set of elements (e.g., \code{matrix, itemMatrix, transactions, 
    itemsets, rules}). }
  \item{y}{ \code{NULL} or a second set to calculate cross dissimilarities. }
  \item{method}{ the distance measure to be used. Implemented measures
  are (defaults to \code{"jaccard"}):
   \describe{
       \item{\code{"affinity"}:}{ measure based on 
         the \code{\link{affinity}},
         a similarity measure between items. It is defined as the
         average \emph{affinity} between the items in two transactions 
         (see Aggarwal et al. (2002)).}
       \item{\code{"cosine"}:}{ the \emph{cosine} distance.} 
       \item{\code{"dice"}:}{ the \emph{Dice's coefficient} defined
         by Dice (1945). Similar to \emph{Jaccard} but gives double the
	     weight to agreeing items.}
       \item{\code{"jaccard"}:}{ the number of items which occur in both 
         elements divided by the total number of items in the 
         elements (Sneath, 1957). 
         This measure is often 
         also called: \emph{binary, asymmetric binary,} etc. }
       \item{\code{"matching"}:}{ the \emph{Matching coefficient} defined
         by Sokal and Michener (1958). This coefficient gives the same
	     weigth to presents and absence of items.}
       \item{\code{"pearson"}:}{ the 1 - \emph{Pearson correlation 
         coefficient} .} 
 }}
  \item{args}{ a list of additional arguments for the methods.  
  
     For calculating
     \code{"affinity"} for associations, the affinities between the items in 
     the transactions are needed and passed to the method as the first 
     element in \code{args}.}
   \item{which}{a character string indicating if the dissimilarity should be 
     calculated between transactions (default) or items (use \code{"items"}). }
   \item{\ldots}{further arguments.}
}
\value{
  returns an object of class \code{dist}.
}
\seealso{
  \code{\link{affinity}},
  \code{\link{dist-class}},
  \code{\link{itemMatrix-class}},
  \code{\link{associations-class}}.
}
\examples{
## cluster items in Groceries with support > 5\%
data("Groceries")

s <- Groceries[,itemFrequency(Groceries)>0.05]
d_jaccard <- dissimilarity(s, which = "items")
plot(hclust(d_jaccard, method = "ward"))



## cluster transactions for a sample of Adult
data("Adult")
s <- sample(Adult, 200) 

##  calculate Jaccard distances and do hclust
d_jaccard <- dissimilarity(s)
plot(hclust(d_jaccard))

## calculate affinity-based distances and do hclust
d_affinity <- dissimilarity(s, method = "affinity")
plot(hclust(d_affinity))


## cluster rules
rules <- apriori(Adult)
rules <- subset(rules, subset = lift > 2)

## we need to supply the item affinities from the dataset (sample)
d_affinity <- dissimilarity(rules, method = "affinity", 
  args = list(affinity = affinity(s)))
plot(hclust(d_affinity))
}
\references{
  Sneath, P. H. A. (1957) Some thoughts on bacterial classification. 
  \emph{Journal of General Microbiology} 17, pages 184--200.
 
  Sokal, R. R. and Michener, C. D. (1958) A statistical method for evaluating 
  systematic relationships. \emph{University of Kansas Science Bulletin} 38, 
  pages 1409--1438.
  
  Dice, L. R. (1945) Measures of the amount of ecologic association 
  between species. \emph{Ecology} 26, pages 297--302.
 
  Charu C. Aggarwal, Cecilia Procopiuc, and Philip S. Yu. (2002)
    Finding localized associations in market basket data.
    \emph{IEEE Trans. on Knowledge and Data Engineering} 14(1):51--62.
}
\keyword{cluster}
\keyword{models}
