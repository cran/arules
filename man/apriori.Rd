\name{apriori}
\alias{apriori}
\title{Mining Associations with Apriori}
\description{
  Mine frequent itemsets, association rules or association hyperedges
  using the Apriori algorithm.  The Apriori algorithm employs level-wise
  search for frequent itemsets.  The implementation of Apriori used
  includes some improvements (e.g., a prefix tree and item sorting).
}
\usage{
apriori(data, parameter = NULL, appearance = NULL, control = NULL)
}
\arguments{
  \item{data}{object of class
    \code{\link[arules:transactions-class]{transactions}} or any data structure
    which can be coerced into
    \code{\link[arules:transactions-class]{transactions}} (e.g., binary
    \code{matrix}, \code{data.frame}).}
  \item{parameter}{object of class
    \code{\link[arules:APparameter-class]{APparameter}} or named list.
    The default behavior is to mine rules with support 0.1, confidence
    0.8, and maxlen 5.}
  \item{appearance}{object of class
    \code{\link[arules:APappearance-class]{APappearance}} or named list.
    With this argument item appearance can be restricted.
    By default all items can appear unrestricted.}
  \item{control}{object of class
    \code{\link[arules:APcontrol-class]{APcontrol}} or named list.
    Controls the performance of the mining algorithm (item sorting, etc.)}
}
\details{
  Calls the C implementation of the Apriori algorithm by Christian
  Borgelt for mining frequent itemsets, rules or hyperedges.
}
\value{
  Returns an object of class \code{\link[arules:rules-class]{rules}} or
  \code{\link[arules:itemsets-class]{itemsets}}.
}
\references{
  R. Agrawal, T. Imielinski, and A. Swami. Mining association rules
  between sets of items in large databases. In \emph{Proceedings of the
    ACM SIGMOD International Conference on Management of Data}, pages
  207-216,
  Washington D.C., May 1993.
  
  Christian Borgelt and Rudolf Kruse. Induction of Association Rules:
  Apriori Implementation. \emph{15th Conference on Computational
    Statistics} (COMPSTAT 2002, Berlin, Germany) Physica Verlag,
  Heidelberg, Germany 2002

  Christian Borgelt. Efficient Implementations of Apriori and
  Eclat.  \emph{Workshop of Frequent Item Set Mining Implementations}
  (FIMI 2003, Melbourne, FL, USA).
}
\examples{
data("Adult")
## Mine association rules.
rules <- apriori(Adult, 
                 parameter = list(supp = 0.5, conf = 0.9,
                                  target = "rules"))
summary(rules)
}
\seealso{
  \code{\link{APparameter-class}},
  \code{\link{APcontrol-class}},
  \code{\link{APappearance-class}},
  \code{\link{transactions-class}},
  \code{\link{itemsets-class}},
  \code{\link{rules-class}}
}
\keyword{models}
