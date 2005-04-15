\name{itemsets-class}
\docType{class}
\alias{itemsets-class}
\alias{summary.itemsets-class}
\alias{show,summary.itemsets-method}
\alias{[,itemsets-method}
\alias{coerce,itemsets,data.frame-method}
\alias{items,itemsets-method}
\alias{items<-,itemsets-method}
\alias{items}
\alias{items<-}
\alias{itemInfo,itemsets-method}
\alias{labels,itemsets-method}
%\alias{length,itemsets-method}
\alias{subset,itemsets-method}
\alias{summary,itemsets-method}
\alias{tidList,itemsets-method}
\alias{tidList}

\title{Class ``itemsets'' --- A Set of Itemsets}
\description{
  The \code{itemsets} class represents a set of itemsets and the
  associated quality measures.
}
\section{Objects from the Class}{
  Objects are the result of calling the functions \code{\link{apriori}}
  (e.g., with \code{target="frequent itemsets"} in the parameter list) 
  or \code{\link{eclat}}.
  Objects can also be created by calls of the form
  \code{new("itemsets", ...)}. 
}
\section{Slots}{
  \describe{
    \item{\code{items}:}{object of class
      \code{\link[itemMatrix-class]{itemMatrix}} containing the 
      items in the set of itemsets}
    \item{\code{quality}:}{object of class \code{"data.frame"}
      containing the quality measures for the itemsets}
    \item{\code{tidList}:}{object of class
      \code{\link[tidList-class]{tidList}} containing the IDs of the
      transactions which support each itemset.  The slot contains
      \code{NULL} if no transactions ID list is available (transactions
      ID lists are only available for \code{\link{eclat}}).
    }
  }
}
\section{Extends}{
  Class \code{\link[associations-class]{associations}}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "itemsets")};
      extracts a subset of itemsets and the associated quality measures}
    \item{coerce}{\code{signature(from = "itemsets", to =
	"data.frame")};
      represent the itemsets in readable form}
    \item{items}{\code{signature(x = "itemsets")};
      returns the \code{\link[itemMatrix-class]{itemMatrix}}
      representing the set of itemsets}
    \item{items<-}{\code{signature(x = "itemsets")};
      replaces the \code{\link[itemMatrix-class]{itemMatrix}}
      representing the set of itemsets}
    \item{itemInfo}{\code{signature(object = "itemsets")}; 
      returns the whole	item information data frame including item
      labels}
    \item{labels}{\code{signature(object = "itemsets")};
      returns labels for the itemsets ("{items}") as a
      \code{\link{character}} vector}
    \item{length}{\code{signature(x = "itemsets")};
      returns the number of itemsets stored in the the set}
    \item{subset}{\code{signature(x = "itemsets")};
      select a subset of itemsets using restrictions on the quality
      measures or on the items present in the itemsets}
    \item{summary}{\code{signature(object = "itemsets")}}
    \item{tidList}{\code{signature(object = "itemsets")};
      returns the transaction ID list}
  }
}
\author{
  Michael Hahsler
}
\seealso{
  \code{\link{eclat}},
  \code{\link{apriori}},
  \code{\link{associations-class}}, 
  \code{\link{tidList-class}} 
}
\examples{
data("Adult_transactions")

## Mine frequent itemsets with Eclat.
fsets <- eclat(Adult_transactions, parameter = list(supp = 0.5))

## Display the 5 itemsets with the highest support.
inspect(SORT(fsets)[1:5])
}
\keyword{classes}
