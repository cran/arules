\name{sets}
\alias{union}
%\alias{union,itemMatrix,itemMatrix-method}
\alias{union,associations,associations-method}
\alias{intersect}
%\alias{intersect,itemMatrix,itemMatrix-method}
\alias{intersect,associations,associations-method}
\alias{setequal}
%\alias{setequal,itemMatrix,itemMatrix-method}
\alias{setequal,associations,associations-method}
\title{Set Operations for Associations}
\description{
  Provides the generic set functions 
  \code{union},  
  \code{intersect} and  
  \code{setequal}  
  and the S4 methods for sets of associations (e.g., rules, itemsets). 
}
\usage{
union(x, y)
%\S4method{union}{associations,associations}(x, y)
%\S4method{union}{itemMatrix,itemMatrix}(x, y)
intersect(x, y)
%\S4method{intersect}{associations,associations}(x, y)
%\S4method{intersect}{itemMatrix,itemMatrix}(x, y)
setequal(x, y)
%\S4method{setequal}{associations,associations}(x, y)
%\S4method{setequal}{itemMatrix,itemMatrix}(x, y)
}
\arguments{
  \item{x,y}{ sets of associations.}
%  \item{x,y}{ a set of associations or an \code{itemMatrix} object.}
}
\value{
   An object of the same class as \code{x} and \code{y}.
}
\seealso{
  \code{\link{associations-class}}
%  \code{\link{itemMatrix-class}}
}
\examples{
data("Adult")


### mine some rules
r <- apriori(Adult)

### take 2 subsets
r1 <- r[1:10]
r2 <- r[6:15]

union(r1,r2)
intersect(r1,r2)
setequal(r1,r2)
}
\keyword{manip}
