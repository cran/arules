\name{tidLists-class}
\docType{class}
\alias{tidLists-class}
%
\alias{tidLists_or_NULL-class}
%\alias{[,tidLists-method}
\alias{coerce,tidLists,matrix-method}
\alias{coerce,tidLists,list-method}
\alias{coerce,tidLists,dgCMatrix-method}
\alias{coerce,tidLists,transactions-method}
\alias{coerce,transactions,tidLists-method}
\alias{coerce,tidLists,itemMatrix-method}
\alias{coerce,itemMatrix,tidLists-method}
\alias{dim,tidLists-method}
\alias{labels,tidLists-method}
\alias{itemLabels,tidLists-method}
\alias{itemInfo,tidLists-method}
\alias{transactionInfo,tidLists-method}
\alias{t,tidLists-method}
\alias{show,tidLists-method}
\alias{summary,tidLists-method}
%
\alias{summary.tidLists-class}
\alias{show,summary.tidLists-method}

\title{Class ``tidLists'' --- Transaction ID Lists for Items/Itemsets}
\description{
  Transaction ID lists contains a set of lists. 
  Each list 
  is associated with an item/itemset and
  stores the IDs of the transactions which support the item/itemset.
  \code{tidLists} uses the class \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}}
  to efficiently store the transaction ID lists as a sparse matrix.
  Each column in the matrix represents one transaction ID list.
  
  \code{tidLists} can be used for different purposes.
  For some operations (e.g., support counting) it is efficient to
  coerce a \code{\link[arules:transactions-class]{transactions}} database 
  into \code{tidLists} where each list contains the transaction IDs for an 
  item (and the support is given by the length of the list).
  
  The implementation of the Eclat mining algorithm (which uses
  transaction ID list intersection) can also produce
  transaction ID lists for the found itemsets as part of the
  returned \code{\link[arules:itemsets-class]{itemsets}} object.
  These lists can then be used for 
  further computation.
  
  
  
}
\section{Objects from the Class}{
  Objects are created by Eclat if the \code{\link{eclat}} function is
  called with \code{tidLists = TRUE} in the
  \code{\link[arules:ECparameter-class]{ECparameter}} object, and returned
  as part of the mined \code{\link[arules:itemsets-class]{itemsets}}.
  Objects can also be created
  by coercion from an object of class 
  \code{\link[arules:transactions-class]{transactions}} or 
  by calls of the form
  \code{new("tidLists", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{object of class
      \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}}.}
     \item{\code{itemInfo}:}{object of class \code{"data.frame"} always an
       empty data.frame for transaction ID lists.}
     \item{\code{transactionInfo}:}{object of class \code{"data.frame"} 
       with vectors of the same length as the number of transactions. 
       Each vector can hold additional information e.g., store
       transaction IDs or user IDs for each transaction.}
   }
}
\section{Methods}{
 \describe{
 % \item{[}{\code{signature(x = "tidLists")};
 %   extracts parts (transaction ID vectors) from the tidLists. 
 %   The argument selects the item or itemset.}
  \item{coerce}{\code{signature(from = "tidLists", to = "dgCMatrix")};
      access the sparse matrix representation. In the
      \code{dgCMatrix} each column represents the transaction IDs
      for one item/itemset.}
  \item{coerce}{\code{signature(from = "tidLists", to = "list")}}
  \item{coerce}{\code{signature(from = "tidLists", to = "matrix")}}
  \item{coerce}{\code{signature(from = "tidLists", to = "dgCMatrix")}}
  \item{coerce}{\code{signature(from = "tidLists", to = "itemMatrix")}}
  \item{coerce}{\code{signature(from = "itemMatrix", to = "tidLists")}}
  \item{coerce}{\code{signature(from = "tidLists", to = "transactions")}}
  \item{coerce}{\code{signature(from = "transactions", to = "tidLists")}}
  \item{dim}{\code{signature(x = "tidLists")};
      returns the dimensions of the sparse Matrix representing the 
      \code{tidLists}.}
  \item{labels}{\code{signature(x = "transactions")};
    returns the labels (item labels and transaction IDs)
    for the incidence matrix as a list of two vectors named \code{items}
    and \code{transactionID}.}
   \item{itemInfo}{ returns the slot \code{itemInfo}.}
  \item{itemLabels}{\code{signature(object = "tidLists")};
    returns the item labels as a character vector.}
  \item{transactionInfo}{\code{signature(x = "transactions")}:
    returns the slot \code{transactionInfo}.}
  \item{show}{\code{signature(object = "tidLists")}}
  \item{summary}{\code{signature(object = "tidLists")}}
}
}
\seealso{
  \code{\link[Matrix]{dgCMatrix}} (in \pkg{Matrix}),
  \code{\link{eclat}},
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}},
  \code{\link{itemsets-class}}
}
\examples{
## Create transaction data set.
data <- list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("b","e"),
  c("b","c","e"),
  c("a","d","e"),
  c("a","c"),
  c("a","b","d"),
  c("c","e"),
  c("a","b","d","e"),
  )
t <- as(data, "transactions")

## Mine itemsets with tidLists.
f <- eclat(data, parameter = list(support = 0, tidLists = TRUE))

## Get dimensions of the tidLists.
dim(tidLists(f))

## Coerce tidLists to list.
as(tidLists(f), "list")
}
\keyword{classes}
