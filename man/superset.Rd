\name{superset}
\alias{is.subset}
\alias{is.subset,itemMatrix,itemMatrix-method}
\alias{is.subset,associations,associations-method}
\alias{is.superset}
\alias{is.superset,itemMatrix,itemMatrix-method}
\alias{is.superset,associations,associations-method}
%
%
\title{Find Super and Subsets}
\description{
  Provides the generic functions 
  and the S4 methods \code{is.subset} and \code{is.superset}
  for finding super or subsets in associations and itemMatrix objects.
}
\usage{
is.subset(set, el, proper = FALSE)
is.superset(set, el, proper = FALSE)
}
\arguments{
  \item{el, set}{ associations or itemMatrix objects.}
  \item{proper}{\code{"logical"}; all or just proper super or subsets.}
}
\details{
looks for supersets (subsets) of \code{el} in \code{set}.
}
\value{
returns a 
logical matrix with \code{length(el)} rows
and \code{length(set)} columns. 
Each logical row vector represents which elements in \code{set}
are supersets (subsets) of the corresponding element in \code{el}
}
\seealso{
  \code{\link{associations-class}},
  \code{\link{itemMatrix-class}}
}
\examples{
data("Adult")
set <- eclat(Adult, parameter = list(supp = 0.8))

### find the supersets of each itemset in set
is.superset(set, set)
}
\keyword{manip}
