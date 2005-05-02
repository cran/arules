\name{tidList-class}
\docType{class}
\alias{tidList-class}
\alias{tidList_or_NULL-class}
\alias{[,tidList-method}
\alias{coerce,tidList,matrix-method}
\alias{coerce,tidList,transactions-method}
\alias{coerce,transactions,tidList-method}
\alias{coerce,tidList,matrix-method}
\alias{coerce,tidList,list-method}
\alias{coerce,tidList,dgCMatrix-method}
\alias{decode,tidList-method}
\alias{labels,tidList-method}
\alias{show,tidList-method}
\alias{summary,tidList-method}
\alias{transactionInfo,tidList-method}
\alias{summary.tidList-class}
\alias{show,summary.tidList-method}

\title{Class ``tidList'' --- A Transaction ID List}
\description{
  The \code{tidList} class is used by the
  \code{\link[itemsets-class]{itemsets}} class to store the IDs of the
  transaction which support each mined itemsets.  Only the
  implementation of the Eclat mining algorithm produces transaction IDs.
  \code{tidList} uses the \code{\link[itemMatrix-class]{itemMatrix}}
  class to efficiently store the transaction ID lists as a sparse matrix.
}
\section{Objects from the Class}{
  Objects are created by Eclat if the \code{\link{eclat}} function is
  called with \code{tidList = TRUE} in the
  \code{\link[ECparameter-class]{ECparameter}} object.
  Objects can also be created by calls of the form
  \code{new("tidList", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{object of class
      \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} (see
      \code{\link[itemMatrix-class]{itemMatrix}})}
     \item{\code{itemInfo}:}{object of class \code{"data.frame"} always an
       empty data.frame for transaction ID lists.}
     \item{\code{transactionInfo}:}{object of class \code{"data.frame"} 
       with vectors of the same length as the number of transactions. 
       Each vector can hold additional information e.g., store
       transaction IDs or user IDs for each transaction.}
   }
}
\section{Methods}{
  \item{[}{\code{signature(x = "tidList")};
    extracts parts (transaction ID vectors) from the tidList. 
    The argument selects the item or itemset.}
  \item{coerce}{\code{signature(from = "tidList", to = "dgCMatrix")};
      access the sparse matrix representation. In the
      \code{dgCMatrix} each column represents the transaction IDs
      for one item/itemset.}
  \item{coerce}{\code{signature(from = "tidList", to = "list")}}
  \item{coerce}{\code{signature(from = "tidList", to = "matrix")}}
  \item{coerce}{\code{signature(from = "tidList", to = "transactions")}}
  \item{coerce}{\code{signature(from = "transactions", to = "tidList")}}
  \item{decode}{\code{signature(x = "tidList")};
    decodes the numeric transaction codes (column numbers in the
    tidList) given in argument \code{tids} to the transaction IDs stored
    in \code{x}.  \code{tids} can be a vector or list.}
  \item{labels}{\code{signature(x = "transactions")};
    returns the labels (item labels and transaction IDs)
    for the incidence matrix as a list of two vectors named \code{items}
    and \code{transactionIDs}.}
  \item{transactionInfo}{\code{signature(x = "transactions")}:
    returns transactionInfo}
  \item{show}{\code{signature(object = "tidList")}}
  \item{summary}{\code{signature(object = "tidList")}}
}
\author{
  Michael Hahsler
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

## Mine itemsets with tidList.
f <- eclat(data, parameter = list(support = 0, tidList = TRUE))

## Get dimensions of the tidList.
dim(tidList(f))

## Coerce tidList to list.
as(tidList(f), "list")
}
\keyword{classes}
