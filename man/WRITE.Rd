\name{WRITE}
\alias{WRITE}
\alias{WRITE,transactions-method}
\alias{WRITE,associations-method}
\title{
  Writes transactions or associations to disk
}
\description{
  Provides the generic function \code{WRITE} and the S4 methods to 
  write transactions or associations to disk.  
}
\usage{
WRITE(x, \dots)
}
\arguments{
  \item{x}{the object.}
  \item{\dots}{further arguments passed on to \code{\link{write.table}}.}
}
\details{
  \code{WRITE} 
  first uses coercion to \code{data.frame} to obtain a printable form
  of \code{x} and then uses \code{\link{write.table}} to write
  the data to disk.
}
\seealso{
 code{\link{write.table}},
 code{\link{transactions-class}},
 code{\link{associations-class}}
}
\examples{
data("Epub")

### write the formated result to screen
WRITE(Epub[1:5])

### write the formated result to file
WRITE(Epub[1:5], file = "data")

unlink("data") # tidy up

}
\keyword{file}
