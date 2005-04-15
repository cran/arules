\name{Epub}
\alias{Epub}
\docType{data}
\title{Epub Data Set}
\description{
  The \code{Epub} data set countains the download history of documents
  from the electronic publication platform of the Vienna University of
  Economics and Business Administration.
  The data was recorded between Jan 2003 and May 2005.
}
\usage{data(Epub)}
\format{
  Object of class \code{\link[transactions-class]{transactions}} with
  2771 transactions and 419 items.
  Item labels are document IDs of the from \code{"epub-wu-01\_11d"}.
  IDs and time stamps for transactions are also provided.
}
\source{
  Provided by Michael Hahsler from ePubWU at
  \url{http://epub.wu-wien.ac.at}.
}
\examples{
data("Epub")
summary(Epub)
}
\keyword{datasets}
