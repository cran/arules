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
\examples{
### create a demo file using basket format for the example
data <- paste("item1,item2","item1","item2,item3", sep="\n")
cat(data)
write(data, file = "demo_basket")

### read demo data
tr <- read.transactions("demo_basket", format = "basket", sep=",")
inspect(tr)


### create a demo file using single format for the example
### column 1 contains the transaction ID and column 2 contains one item
data <- paste("trans1 item1", "trans2 item1","trans2 item2", sep ="\n")
cat(data)
write(data, file = "demo_single")

### read demo data
tr <- read.transactions("demo_single", format = "single", cols = c(1,2))
inspect(tr)


### tidy up
unlink("demo_basket")
unlink("demo_single")

}
\keyword{file}
