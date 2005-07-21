\name{read.transactions}
\alias{read.transactions}
\title{Read Transaction Data}
\description{
  Reads a transaction data file from disk and creates a
  \code{\link[arules:transactions-class]{transactions}} object.
}
\usage{
read.transactions(file, format = c("basket", "single"), sep = NULL,
                  cols = NULL)
}
\arguments{
  \item{file}{the file name.}
  \item{format}{a character string indicating the format of the data
    set.  One of \code{"basket"} or \code{"single"}, can be
    abbreviated.}
  \item{sep}{a character string specifying how fields are separated in
    the data file, or \code{NULL} (default).  For basket format, this
    can be a regular expression; otherwise, a single character must be
    given.  The default corresponds to white space separators.}
  \item{cols}{an numeric vector of length two giving the numbers of the
    columns (fields) with the transaction and item ids, respectively.
    Only used for \sQuote{single} format.}
}
\details{
  For \sQuote{basket} format, each line in the transaction data file
  represents a transaction where the items (item labels) are separated
  by the characters specified by \code{sep}.  For \sQuote{single}
  format, each line corresponds to a single item, containing at least
  ids for the transaction and the item.
}
\value{
  Returns an object of class
  \code{\link[arules:transactions-class]{transactions}}.
}
\seealso{
  \code{\link{transactions-class}}
}
\keyword{file}
