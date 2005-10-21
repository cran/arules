\name{APappearance-class}
\docType{class}
\alias{APappearance-class}
\alias{coerce,list,APappearance-method}
\alias{coerce,NULL,APappearance-method}
\title{Class ``APappearance'' --- Specifying the `appearance' Argument of
  apriori()}

\description{Specifies the restrictions on the associations mined by
  \code{\link{apriori}}.  Note that appearance is not supported by the
  implementation of \code{\link{eclat}}.
}

\section{Objects from the Class}{
  If appearance restrictions are used, an appearance object will be
  created automatically within the \code{\link{apriori}} function using
  the information in the named list of the function's \code{appearance}
  argument.  In this case, the item labels used in the list will be
  automatically matched against the items in the used transaction database.
  The list can contain the following elements:

  \describe{
    \item{\code{default}:}{one of \code{"both"}, \code{"lhs"},
      \code{"rhs"}, \code{"none"} (the default is \code{"both"}).  This
      element specified the default appearance for all items not 
      explicitly mentioned in the other elements of the list.}
    \item{\code{lhs}, \code{rhs}, \code{both}, \code{none},
      \code{items}:}{character vectors giving the labels of the items
      which may only appear in the corresponding place of the
      rules/itemsets.}
  }

  Objects can also be created by calls of the form 
  \code{new("APappearance", ...)}.  In this case, item IDs (column
  numbers of the transactions incidence matrix) have to be used instead
  of labels.
}
\section{Slots}{
  \describe{
    \item{\code{set}:}{\code{"integer"};
      indicates how many items are specified for each of lhs, rhs,
      items, both and none}
    \item{\code{items}:}{\code{"integer"};
      vector of item IDs (column numbers)}
    \item{\code{labels}:}{\code{"character"};
      vector of item labels}
    \item{\code{default}:}{\code{"character"};
      indicates the value for default appearance}
    }
}
\references{
  Christian Borgelt (2004).
  \emph{Apriori --- Finding Association Rules/Hyperedges with the Apriori
    Algorithm}.
  \url{http://fuzzy.cs.uni-magdeburg.de/~borgelt/apriori.html}
}
\examples{
data("Adult")
## Mine only rules with small or large income in the right-hand-side.
rules <- apriori(Adult, parameter = list(confidence = 0.5), 
  appearance = list(rhs = c("income=small", "income=large"),
  default="lhs"))
}
\seealso{
  \code{\link{apriori}},
  \code{\link{transactions-class}}
}
\keyword{classes}
