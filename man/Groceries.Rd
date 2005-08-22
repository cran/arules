\name{Groceries}
\alias{Groceries}
\docType{data}
\title{Groceries Data Set}
\description{
  The \code{Groceries} data set contains 1 month (30 days) of real-world
  point-of-sale transaction data from a typical local grocery outlet.  The data
  set contains 9835 transactions and the items are aggregated to 169
  categories.
}
\usage{data(Groceries)}
\format{
Object of class \code{transactions}.
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
  The data set is provided for arules by Michael Hahsler, Kurt Hornik 
  and Thomas Reutterer.
}
\references{
Michael Hahsler, Kurt Hornik, and Thomas Reutterer. Implications of
probabilistic data modeling for mining association rules. In Proceedings of the
29th Annual Conference of the Gesellschaft fuer Klassifikation e.V., University
of Magdeburg, March 9-11, 2005, Studies in Classification, Data Analysis, and
Knowledge Organization. Springer-Verlag, 2005. Accepted for publication.
}
\examples{
data(Groceries)

summary(Groceries)
}
\keyword{datasets}
