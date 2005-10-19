\name{eclat}
\alias{eclat}
\title{Mining Associations with Eclat}
\description{
  Mine frequent itemsets with the Eclat algorithm.
  This algorithm uses simple intersection operations for equivalence
  class clustering along with bottom-up lattice traversal.
}
\usage{
eclat(data, parameter = NULL, control = NULL)
}
\arguments{
 \item{data}{object of class
   \code{\link[arules:transactions-class]{transactions}} or any data structure
   which can be coerced into
   \code{\link[arules:transactions-class]{transactions}} (e.g., binary
   \code{matrix}, \code{data.frame}).}
 \item{parameter}{object of class
   \code{\link[arules:ECparameter-class]{ECparameter}} or named list (default
   values are: support 0.1 and maxlen 5)}
  \item{control}{Object of class
    \code{\link[arules:ECcontrol-class]{ECcontrol}} or named list for
    algorithmic controls.}
}
\details{
  Calls the C implementation of the Eclat algorithm by Christian 
  Borgelt for mining frequent itemsets. 
}
\value{
  Returns an object of class \code{\link[arules:itemsets-class]{itemsets}}.
}
\references{
  Mohammed J. Zaki, Srinivasan Parthasarathy, Mitsunori Ogihara, and Wei
  Li.
  \emph{New algorithms for fast discovery of association rules}.
  Technical Report 651, Computer Science Department, University of
  Rochester, Rochester, NY 14627, July 1997.
  
  Christian Borgelt. Efficient Implementations of Apriori and
  Eclat. \emph{Workshop of Frequent Item Set Mining Implementations}
  (FIMI 2003, Melbourne, FL, USA).
}
\seealso{
  \code{\link{ECparameter-class}},
  \code{\link{ECcontrol-class}},
  \code{\link{transactions-class}},
  \code{\link{itemsets-class}},
  \code{\link{apriori}}
}
\examples{
data("Adult")
## Mine itemsets with minimum support of 0.1.
itemsets <- eclat(Adult,
                  parameter = list(supp = 0.1, maxlen = 15))
}
\keyword{models}
