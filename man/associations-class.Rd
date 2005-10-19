\name{associations-class}
\docType{class}
\alias{associations-class}
%
\alias{summary.associations-class}
\alias{show,associations-method}
%\alias{SORT,associations-method}
\alias{quality<-}
\alias{quality<-,associations-method}
\alias{quality}
\alias{quality,associations-method}
\alias{items,associations-method}
\alias{labels,associations-method}
\alias{length,associations-method}
\alias{t,associations-method}


\title{Class ``associations'' - A Set of Associations}
\description{
  The \code{associations} class is a virtual class which is extended to
  represent mining result (e.g., sets of itemsets or rules).  The class
  provides accessors for the quality slot and a method for sorting the
  associations.
}
\section{Objects from the Class}{
  A virtual class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{quality}:}{Object of class \code{data.frame} for quality
      measures (e.g., interest measures as support or confidence).  Each
      quality measure is a named vector with the same length as the
      number of elements in the set of associations and each vector
      element belongs to the association with the same index.}
  }
}
\section{Methods}{
  \describe{
    \item{items}{\code{signature(x = "associations")};
      dummy method. This method has to be implemented by all subclasses
      of \code{associations} and return the items which make up each
      association as an object of class 
      \code{itemMatrix}.}
    \item{labels}{\code{signature(object = "associations")};
      dummy method. This method has to be implemented by all subclasses
      of \code{associations} and return a vector
      of \code{length(object)} of labels
      for the elements in the association.}
    \item{length}{\code{signature(x = "associations")};
      dummy method. This method has to be implemented by all subclasses
      of \code{associations} and return the number of elements in the 
      association.}
%    \item{SORT}{\code{signature(x = "associations")};
%      sorts the associations by a quality measure specified as the
%      argument \code{by} (default: \code{by = "support", decreasing =
%	TRUE}).}
    \item{quality<-}{\code{signature(x = "associations")};
      replaces the quality \code{data.frame}.  The length of the vectors
      in the \code{data.frame} has to equal the number of associations
      in the set.}
    \item{quality}{\code{signature(x = "associations")};
      returns the quality \code{data.frame}.}
    \item{show}{\code{signature(object = "associations")}}
  }
}
\section{Subclasses}{
  \code{\link{itemsets-class}},
  \code{\link{rules-class}}
}
\seealso{
  \code{\link{itemMatrix-class}}
}
\keyword{classes}
