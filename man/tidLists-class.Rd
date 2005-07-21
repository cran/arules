\name{tidLists-class}
\docType{class}
\alias{tidLists-class}
\alias{tidLists_or_NULL-class}
\alias{[,tidLists-method}
\alias{coerce,tidLists,matrix-method}
\alias{coerce,tidLists,transactions-method}
\alias{coerce,transactions,tidLists-method}
\alias{coerce,tidLists,matrix-method}
\alias{coerce,tidLists,list-method}
\alias{coerce,tidLists,dgCMatrix-method}
\alias{decode,tidLists-method}
\alias{labels,tidLists-method}
\alias{show,tidLists-method}
\alias{summary,tidLists-method}
\alias{transactionInfo,tidLists-method}
\alias{summary.tidLists-class}
\alias{show,summary.tidLists-method}

\title{Class ``tidLists'' --- A Transaction ID List}
\description{
  The \code{tidLists} class is used by the
  \code{\link[arules:itemsets-class]{itemsets}} class to store the IDs of the
  transaction which support each mined itemsets.  Only the
  implementation of the Eclat mining algorithm produces transaction IDs.
  \code{tidLists} uses the \code{\link[arules:itemMatrix-class]{itemMatrix}}
  class to efficiently store the transaction ID lists as a sparse matrix.
}
\section{Objects from the Class}{
  Objects are created by Eclat if the \code{\link{eclat}} function is
  called with \code{tidLists = TRUE} in the
  \code{\link[arules:ECparameter-class]{ECparameter}} object.
  Objects can also be created by calls of the form
  \code{new("tidLists", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{object of class
      \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} (see
      \code{\link[arules:itemMatrix-class]{itemMatrix}})}
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
  \item{[}{\code{signature(x = "tidLists")};
    extracts parts (transaction ID vectors) from the tidLists. 
    The argument selects the item or itemset.}
  \item{coerce}{\code{signature(from = "tidLists", to = "dgCMatrix")};
      access the sparse matrix representation. In the
      \code{dgCMatrix} each column represents the transaction IDs
      for one item/itemset.}
  \item{coerce}{\code{signature(from = "tidLists", to = "list")}}
  \item{coerce}{\code{signature(from = "tidLists", to = "matrix")}}
  \item{coerce}{\code{signature(from = "tidLists", to = "transactions")}}
  \item{coerce}{\code{signature(from = "transactions", to = "tidLists")}}
  \item{decode}{\code{signature(x = "tidLists")};
    decodes the numeric transaction codes (column numbers in the
    tidLists) given in argument \code{tids} to the transaction IDs stored
    in \code{x}.  \code{tids} can be a vector or list.}
  \item{labels}{\code{signature(x = "transactions")};
    returns the labels (item labels and transaction IDs)
    for the incidence matrix as a list of two vectors named \code{items}
    and \code{transactionIDs}.}
  \item{transactionInfo}{\code{signature(x = "transactions")}:
    returns transactionInfo}
  \item{show}{\code{signature(object = "tidLists")}}
  \item{summary}{\code{signature(object = "tidLists")}}
}
}
\seealso{
  \code{\link{eclat}},
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
