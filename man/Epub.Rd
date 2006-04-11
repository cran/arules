\name{Epub}
\alias{Epub}
\docType{data}
\title{Epub Data Set}
\description{
  The \code{Epub} data set countains the download history of documents
  from the electronic publication platform of the Vienna University of
  Economics and Business Administration.
  The data was recorded between Jan 2003 and Mar 2006.
}
\usage{data(Epub)}
\format{
  Object of class \code{\link[arules:transactions-class]{transactions}} with
  3975 transactions and 465 items.
  Item labels are document IDs of the from \code{"doc\_11d"}.
  Session IDs and time stamps for transactions are also provided.
}
\source{
  Provided by Michael Hahsler from ePub-WU at
  \url{http://epub.wu-wien.ac.at}.
}
\examples{
data("Epub")
summary(Epub)
}
\keyword{datasets}
