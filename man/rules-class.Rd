\name{rules-class}
\docType{class}
\alias{rules-class}
\alias{summary.rules-class}
\alias{show,summary.rules-method}
\alias{[,rules-method}
\alias{coerce,rules,data.frame-method}
\alias{itemInfo,rules-method}
\alias{labels,rules-method}
%\alias{length,rules-method}
\alias{lhs<-,rules-method}
\alias{lhs,rules-method}
\alias{rhs<-,rules-method}
\alias{rhs,rules-method}
\alias{lhs<-}
\alias{lhs}
\alias{rhs<-}
\alias{rhs}
\alias{subset,rules-method}
\alias{summary,rules-method}

\title{Class ``rules'' --- A Set of Rules}
\description{
  The \code{rules} class represents a set of rules.
}
\section{Objects from the Class}{
  Objects are the result of calling the function \code{\link{apriori}}.
  Objects can also be created by calls of the form
  \code{new("rules", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{lhs}:}{Object of class
      \code{\link[itemMatrix-class]{itemMatrix}}; 
      the left-hand-sides of the rules (antecedents)}
    \item{\code{rhs}:}{Object of class
      \code{\link[itemMatrix-class]{itemMatrix}};
      the right-hand-sides of the rules (consequents)} 
    \item{\code{quality}:}{Object of class \code{"data.frame"}}
  }
}
\section{Extends}{
  Class \code{\link[associations-class]{associations}}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "rules")};
      extracts a subset of rules and the associated quality measures}
    \item{coerce}{\code{signature(from = "rules", to = "data.frame")}; 
      represents the set of rules as a \code{data.frame}}
    \item{itemInfo}{\code{signature(object = "rules")};  
      returns the whole item information data frame including item
      labels}
    \item{labels}{\code{signature(object = "rules")};
      returns labels for the rules ("{lhs} => {rhs}") as a
      \code{character} vector} 
    \item{length}{\code{signature(x = "rules")};
      returns the number of rules stored in the the set}
    \item{lhs}{\code{signature(x = "rules")};
      returns the \code{\link[itemMatrix-class]{itemMatrix}}
      representing the left-hand-side of the rules (antecedents)}
    \item{lhs<-}{\code{signature(x = "rules")};
      replaces the \code{\link[itemMatrix-class]{itemMatrix}}
      representing the left-hand-side of the rules (antecedents)}
    \item{rhs}{\code{signature(x = "rules")};
      returns the \code{\link[itemMatrix-class]{itemMatrix}}
      representing the right-hand-side of the rules (consequents)}
    \item{rhs<-}{\code{signature(x = "rules")};
      replaces the \code{\link[itemMatrix-class]{itemMatrix}}
      representing the right-hand-side of the rules (consequents)}
    \item{subset}{\code{signature(x = "rules")};
      selects a subset using restrictions on the quality measures or on
      the items present in the rules (see examples).}
    \item{summary}{\code{signature(object = "rules")}}
  }
}
\author{
  Michael Hahsler
}
\seealso{
  \code{\link{apriori}},
  \code{\link{associations-class}},
  \code{\link{itemMatrix-class}},
}
\examples{
data("Adult_transactions")

## Mine rules.
rules <- apriori(Adult_transactions, parameter = list(support = 0.4))

## Select a subset using an item in the right-hand-side
## and a quality measure/
rules.sub <- subset(rules, subset = rhs \%in\% "sex" & lift > 1.3)

## Display rules.
inspect(SORT(rules.sub)[1:3])
}
\keyword{classes}
