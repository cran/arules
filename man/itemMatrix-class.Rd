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
\alias{\%pin\%}
\alias{\%pin\%,itemMatrix-method}
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
\alias{nitems}
\alias{nitems,itemMatrix-method}
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
      category information associated with each item label.  
      %This
      %information is used for the \code{\%in\%} method in this
      %class.  The subset method in associations can use this method to
      %select subsets of associations containing items with matching
      %variable or category names.
    }
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "matrix", to = "itemMatrix")}};
      expects \code{from} to be a binary matrix only containing 0s and 1s.
    \item{coerce}{\code{signature(from = "list", to = "itemMatrix")}};
      \code{from} is a list of vectors. Each vector contains one 
      set/transaction/\ldots.
    \item{coerce}{\code{signature(from = "itemMatrix", to = "dgCMatrix")}};
      access the sparse matrix representation. Note, the \code{dgCMatrix} 
      contains a transposed from of the itemMatrix.
    \item{coerce}{\code{signature(from = "itemMatrix", to = "matrix")}; coerces
      to a dense 0-1 matrix of \code{storage.mode} \code{"integer"} instead of 
	\code{"double"} to save memory.}
    \item{coerce}{\code{signature(from = "itemMatrix", to = "list")}}; see also
      the methods for \code{LIST}.
    \item{dim}{\code{signature(x = "itemMatrix")};
      returns the dimensions of the itemMatrix.}
    \item{\%in\%}{\code{signature(x = "itemMatrix")}: matches values 
      against the item labels for 
      each element (row) in the itemMatrix.}
    \item{\%pin\%}{\code{signature(x = "itemMatrix")}: matches values 
      against the item labels for 
      each element (row) in the itemMatrix using partial matching.}
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
      and \code{elements}. The following arguments can be used to customize
      the representation of the elements: 
      \code{itemSep}, 
      \code{setStart} and 
      \code{setEnd}. 
      }
    \item{nitems}{\code{signature(x = "itemMatrix")}; returns the number 
      of items (number if columns) in the itemMatrix.}
    \item{show}{\code{signature(object = "itemMatrix")}}
    \item{summary}{\code{signature(object = "itemMatrix")}}
  }
}
\seealso{
  \code{\link{LIST}},
  \code{\link{c}},
  \code{\link{duplicated}},
  \code{\link{inspect}},
  \code{\link{is.subset}},
  \code{\link{is.superset}},
  \code{\link{itemFrequency}},
  \code{\link{itemFrequencyPlot}},
  \code{\link{match}},
  \code{\link{length}},
  \code{\link{sets}},
  \code{\link{subset}},
  \code{\link{unique}},
  \code{\link{[-methods}},
  \code{\link{image}},
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
