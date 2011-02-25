\name{WRITE}
\alias{WRITE}
\alias{write}
\alias{write.csv}
\alias{write.table}
\alias{WRITE,transactions-method}
\alias{WRITE,associations-method}
\title{
  Writes transactions or associations to disk
}
\description{
  Provides the generic function \code{WRITE} and the S4 methods to 
  write transactions or associations (itemsets, rules) to disk.  
}
\usage{
WRITE(x, file = "",\dots)
\S4method{WRITE}{transactions}(x, file="", format = c("basket", "single"),
                sep=" ", quote=FALSE, ...)
\S4method{WRITE}{associations}(x, file="", sep=" ", quote=FALSE, ...)
}
\arguments{
  \item{x}{the transactions or associations (rules, itemsets, etc.) object.}
  \item{file}{either a character string naming a file or a connection open
            for writing.  '""' indicates output to the console.}
 \item{format}{format to write transactions.}
 \item{sep}{the field separator string. Values within each row of x
           are separated by this string. Use \code{quote=TRUE} and 
	   \code{sep=","} 
	   for saving data as in csv format.}
 \item{quote}{a logical value. Quote fields?}
  \item{\dots}{further arguments passed on to \code{\link{write.table}}
  or \code{\link{write}}.}
}
\details{
  For associations (rules and itemsets) \code{WRITE} first uses coercion 
  to data.frame to obtain a printable form of
  \code{x} and then uses \code{\link{write.table}} to write the data to disk.

  Transactions can be saved in basket (one line per transaction) or in 
  single (one line per item) format. 

  Note: To save and load associations in compact form, use \code{save} and
  \code{load} from the \pkg{base} package.  Alternatively, association can be
  written to disk in PMML (Predictive Model Markup Language). This requires
  the packages \pkg{pmml} and \pkg{XML}. See Examples section for usage.
}
\seealso{
\code{\link{write.table}} (in \pkg{base}),
 \code{\link{transactions-class}},
 \code{\link{associations-class}}
}
\examples{
data("Epub")

## write the formated transactions to screen (basket format)
WRITE(head(Epub))

## write the formated transactions to screen (single format)
WRITE(head(Epub), format="single")

## write the formated result to file in CSV format
WRITE(Epub, file = "data.csv", format="single", quote=TRUE, 
    sep = ",", col.names = NA)

## write rules in CSV format
rules <- apriori(Epub, parameter=list(support=0.0005, conf=0.8))
WRITE(rules, file = "data.csv", quote=TRUE, sep = ",", col.names = NA)

unlink("data.csv") # tidy up

## write rules as PMML
library(pmml)
rules_pmml <- pmml(rules)
saveXML(rules_pmml, "data.xml")

unlink("data.xml") # tidy up
}
\keyword{file}
