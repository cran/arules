\name{dissimilarity}
\alias{dissimilarity}
\alias{dissimilarity,matrix-method}
\alias{dissimilarity,itemMatrix-method}
\alias{dissimilarity,associations-method}
\title{
  Dissimilarity Computation
}
\description{
  Provides the generic function \code{dissimilarity} and the S4 methods
  to compute and returns distances 
  for binary data in a \code{matrix},
  \code{\link[arules:transactions-class]{transactions}} or 
  \code{\link[arules:associations-class]{associations}}.
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
  are (defaults to \code{"Jaccard"}):
   \describe{
       \item{\code{"Jaccard"}:}{ the number of items which occur in both 
         elements divided by the total number of items in the 
         elements (Sneath, 1957). 
         This measure is often 
         also called: \emph{binary, asymmetric binary,} etc. }
       \item{\code{"Matching"}:}{ the \emph{Matching coefficient} defined
         by Sokal and Michener (1958). This coefficient gives the same
	 weigth to presents and absence of items.}
       \item{\code{"Dice"}:}{ the \emph{Dice's coefficient} defined
         by Dice (1945). Similar to \emph{Jaccard} but gives double the
	 weight to agreeing items.}
       \item{\code{"Affinity"}:}{ measure based on 
         the \code{\link{affinity}}},
         a similarity measure between items. It is defined as the
         average \emph{affinity} between the items in two transactions 
         (see Aggarwal et al. (2002)).}
 }
  \item{args}{ a list of additional arguments for the methods.  
  
     For calculating
     \code{"Affinity"} for associations, the affinities between the items in 
     the transactions are needed and passed to the method as the first 
     element in \code{args}.}
   \item{which}{a string indicating if the dissimilarity should be 
     calculated between transavtions (default) or items (use \code{"items"}). }
   \item{\ldots}{further arguments.}
}
\value{
  returns an object of class \code{dist}.
}
\seealso{
  \code{\link{affinity}},
  \code{\link{dist-class}},
  \code{\link[arules]{itemMatrix-class}},
  \code{\link[arules]{associations-class}}.
}
\examples{
data("Adult")

## cluster transactions
## choose a sample 
s <- sample(Adult, 500) 

##  calculate Jaccard distances and do hclust
d_jaccard <- dissimilarity(s)
plot(hclust(d_jaccard))

## calculate affinity-based distances and do hclust
d_affinity <- dissimilarity(s, method = "Affinity")
plot(hclust(d_affinity))


## cluster rules
## mine rules
rules <- apriori(Adult)
## choose promising rules
rules <- subset(rules, subset = lift > 2)

## we need to supply the item affinities from the dataset (sample)
d_affinity <- dissimilarity(rules, method = "Affinity", 
  args = list(affinity = affinity(s)))
plot(hclust(d_affinity))
}
\references{
  Sneath, P. H. A. Some thoughts on bacterial classification. 
  Journal of General Microbiology 17, pages 184-200, 1957.
 
  Sokal, R. R. and Michener, C. D. A statistical method for evaluating 
  systematic relationships. University of Kansas Science Bulletin 38, 
  pages 1409-1438, 1958
  
  Dice, L. R. Measures of the amount of ecologic association 
  between species. Ecology 26, pages 297-302, 1945.
 
  Charu C. Aggarwal, Cecilia Procopiuc, and Philip S. Yu.
    Finding localized associations in market basket data.
  IEEE Trans. on Knowledge and Data Engineering, 14(1), pages 51-62, 2002.
}
\keyword{cluster}
