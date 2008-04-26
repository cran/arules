\name{WRITE}
\alias{WRITE}
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
}
\arguments{
  \item{x}{the transactions or associations (rules, itemsets, etc.) object.}
  \item{file}{either a character string naming a file or a connection open
            for writing.  '""' indicates output to the console.}
  \item{\dots}{further arguments passed on to \code{\link{write.table}}.}
}
\details{
  \code{WRITE} first uses coercion to data.frame to obtain a printable form of
  \code{x} and then uses \code{\link{write.table}} to write the data to disk.

  Note: To save and load associations in compact form, use \code{save} and
  \code{load} from the \pkg{base} package.  Alternatively, association can be
  written to disk in PMML (Predictive Modelling Markup Language). This requires
  the packages \pkg{pmml} and \pkg{XML}. See Examples section for usage.
}
\seealso{
\code{\link{write.table}} (in \pkg{base}),
 \code{\link{transactions-class}},
 \code{\link{associations-class}}
}
\examples{
data("Epub")

## write the formated result to screen
WRITE(head(Epub))

## write the formated result to file in CSV format
WRITE(Epub, file = "data.csv", sep = ",", col.names = NA)

## write rules in CSV format
rules <- apriori(Epub, parameter=list(support=0.002, conf=0.8))
WRITE(rules, file = "data.csv", sep = ",", col.names = NA)

unlink("data.csv") # tidy up

## write rules as PMML
library(pmml)
rules_pmml <- pmml(rules)
saveXML(rules_pmml, "data.xml")

unlink("data.xml") # tidy up
}
\keyword{file}
