\name{itemMatrix-class}
\docType{class}
\alias{itemMatrix-class}
\alias{summary.itemMatrix-class}
\alias{show,summary.itemMatrix-method}
\alias{print,summary.itemMatrix-method}
%\alias{[,itemMatrix-method}
%\alias{LIST,itemMatrix-method}
%\alias{LIST}
\alias{coerce,matrix,itemMatrix-method}
\alias{coerce,list,itemMatrix-method}
\alias{coerce,itemMatrix,matrix-method}
\alias{coerce,itemMatrix,dgCMatrix-method}
\alias{coerce,itemMatrix,list-method}
\alias{dim,itemMatrix-method}
%\alias{image,itemMatrix-method}
\alias{\%in\%,itemMatrix-method}
\alias{itemLabels<-,itemMatrix-method}
\alias{itemLabels,itemMatrix-method}
\alias{itemLabels<-}
\alias{itemLabels}
\alias{labels,itemMatrix-method}
\alias{labels}
\alias{itemInfo<-,itemMatrix-method}
\alias{itemInfo,itemMatrix-method}
\alias{itemInfo<-}
\alias{itemInfo}
%\alias{length,itemMatrix-method}
\alias{show,itemMatrix-method}
\alias{summary,itemMatrix-method}
%%% some additional stuff for dgCMatrix
%\alias{[}
%\alias{[,dgCMatrix-method}

\title{Class ``itemMatrix'' --- Sparse Binary Incidence Matrix to Represent Sets
  of Items}
\description{The \code{itemMatrix} class is the basic building block
  for transactions, itemsets and rules in package \pkg{arules}. 
  The class contains a sparse Matrix representation of items (a set of
  itemsets or transactions) and the corresponding item labels.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("itemMatrix",
    ...)}.
  However, most of the time objects will be created by coercion from a
  \code{matrix}, \code{list} or \code{data.frame}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class
      \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} 
      (from package \pkg{Matrix}) which stores 
      item occurrences in sparse representation.  Note that the 
      \code{dgCMatrix} is column-oriented and \code{itemMatrix} is
      row-oriented with each row representing an element (an itemset, a
      transaction, etc.).  As a result, the \code{dgCMatrix} in this
      slot is always a transposed version of the binary incidence matrix
      in \code{itemMatrix}.
    }
    \item{\code{itemInfo}:}{Object of class \code{"data.frame"} which
      contains named vectors of the length equal to the number of
      elements in the set.  If the slot is not empty (contains no item
      labels), the first element in the \code{data.frame} must have the
      name \code{"labels"} and contain a character vector with the item
      labels used for representing an item.  In addition to the item
      labels, the \code{data.frame} can contain arbitrary named vectors
      (of the same length) to represent, e.g., variable names and values
      which were used to create the binary items or hierarchical
      category information associated with each item label.  This
      information is used for the \code{\%in\%} method in this
      class.  The subset method in associations can use this method to
      select subsets of associations containing items with matching
      variable or category names.
    }
  }
}
\section{Methods}{
  \describe{
%    \item{[}{\code{signature(x = "itemMatrix")};
%      extracts parts of the \code{itemMatrix}.  The first argument
%      selects rows (e.g., transactions or rules) and the second argument
%      selects columns (items). 
%      Either argument can be omitted to select all rows or columns.}
%    \item{LIST}{\code{signature(from = "itemMatrix")}:
%      the internal method used for the coercion to \code{list}.
%      It returns the object \code{from} 
%      as a list of item vectors.
%      Each vector represents the items in one row 
%      (e.g., transaction or itemset). 
%      The items are represented by default by the item labels
%      (equal to coercion to list), however
%      with the argument \code{decode = FALSE} the items can be
%      represented in the list by the corresponding
%      column numbers in the \code{itemMatrix} instead.}
    \item{coerce}{\code{signature(from = "matrix", to = "itemMatrix")}}
    \item{coerce}{\code{signature(from = "list", to = "itemMatrix")}}
    \item{coerce}{\code{signature(from = "itemMatrix", to = "dgCMatrix")}};
      access the sparse matrix representation. Note, the \code{dgCMatrix} 
      contains a transposed from of the itemMatrix.
    \item{coerce}{\code{signature(from = "itemMatrix", to = "matrix")}}
    \item{coerce}{\code{signature(from = "itemMatrix", to = "list")}}
%    \item{decode}{\code{signature(x = "itemMatrix")};
%      decodes the numeric items codes (column numbers in the itemMatrix)
%      given in argument \code{items} to the item names using the labels
%      stored in \code{x}.
%      \code{items} can be a vector or list.}
    \item{dim}{\code{signature(x = "itemMatrix")};
      returns the dimensions of the itemMatrix.}
%    \item{image}{\code{signature(x = "itemMatrix")};
%      plots an image of the itemMatrix for visual inspection.}
    \item{\%in\%}{\code{signature(x = "itemMatrix")}: matches values 
      against the item labels (and the additional information) for 
      each element (row) in the itemMatrix.}
    \item{itemLabels}{\code{signature(object = "itemMatrix")};
      returns the item labels used for encoding as a character vector.}
    \item{itemLabels<-}{\code{signature(object = "itemMatrix")};
      replaces the item labels used for encoding.}
    \item{itemInfo}{\code{signature(object = "itemMatrix")};
      returns the whole item information \code{data.frame} including
      labels.}
    \item{itemInfo<-}{\code{signature(object = "itemMatrix")};
      replaces the item info by a \code{data.frame}.
      The length of the vectors in the \code{data.frame} has to match
      the number of elements (rows) in the itemMatrix.}
    \item{labels}{\code{signature(x = "transactions")};
      returns the labels (item labels and element names) 
      for the matrix as a list of two vectors named \code{items}
      and \code{elements}.}
%    \item{length}{\code{signature(x = "itemMatrix")};
%      returns the number of elements (rows) in the itemMatrix.}
    \item{show}{\code{signature(object = "itemMatrix")}}
    \item{summary}{\code{signature(object = "itemMatrix")}}
  }
}
\seealso{
  \code{\link[Matrix]{dgCMatrix-class}} (from \pkg{Matrix}),
  \code{\link{transactions-class}},
  \code{\link{itemsets-class}},
  \code{\link{rules-class}}
}
\examples{
## Generate random data and coerce data to itemMatrix.
m <- matrix(as.integer(runif(100000)>0.8), ncol=20)
dimnames(m) <- list(NULL, paste("item", c(1:20), sep=""))
i <- as(m, "itemMatrix")

## Get the number of elements (rows) in the itemMatrix.
length(i)

## Get first 5 elements (rows) of the itemMatrix as list.
as(i[1:5], "list")

## Get first 5 elements (rows) of the itemMatrix as matrix.
as(i[1:5], "matrix")

## Get first 5 elements (rows) of the itemMatrix as sparse dgCMatrix.
##  Warning: for efficiency reasons, the dgCMatrix you get is transposed!
as(i[1:5], "dgCMatrix")
}
\keyword{classes}
