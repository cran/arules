% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_write.R
\name{write}
\alias{write}
\alias{write.csv}
\alias{write,transactions-method}
\alias{write,associations-method}
\title{Write Transactions or Associations to a File}
\usage{
write(x, file = "", ...)

\S4method{write}{transactions}(
  x,
  file = "",
  format = c("basket", "single"),
  sep = " ",
  quote = TRUE,
  ...
)

\S4method{write}{associations}(x, file = "", sep = " ", quote = TRUE, ...)
}
\arguments{
\item{x}{the \link{transactions} or \link{associations} (\link{rules}, \link{itemsets}, etc.) object.}

\item{file}{either a character string naming a file or a connection open for
writing.  '""' indicates output to the console.}

\item{\dots}{further arguments passed on to \code{\link[=write.table]{write.table()}}.
Use \code{fileEncoding} to set the encoding used for
writing the file.}

\item{format}{format to write transactions.}

\item{sep}{the field separator string. Values within each row of x are
separated by this string. Use \code{quote = TRUE} and \code{sep = ","} for
saving data as in csv format.}

\item{quote}{a logical value. Quote fields?}
}
\description{
Provides the generic function \code{write()} and the methods to write
\link{transactions} or \link{associations} to a file.
}
\details{
For associations (\link{rules} and \link{itemsets}) \code{write()} first uses coercion to
data.frame to obtain a printable form of \code{x} and then uses
\code{\link[utils:write.table]{utils::write.table()}} to write the data to disk. This is just a method to
export the rules in human-readable form. These exported associations cannot be
read back in as rules. To save and load associations in compact form, use \code{\link[=save]{save()}} and
\code{\link[=load]{load()}} from the \pkg{base} package.  Alternatively, association can be
written to disk in PMML (Predictive Model Markup Language) via
\code{\link[=write.PMML]{write.PMML()}}. This requires package \pkg{pmml}.

Transactions can be saved in \emph{basket} (one line per transaction) or in \emph{single}
(one line per item) format.
}
\examples{
data("Epub")

## write the formated transactions to screen (basket format)
write(head(Epub))

## write the formated transactions to screen (single format)
write(head(Epub), format = "single")

## write the formated result to file in CSV format
write(Epub, file = "data.csv", format = "single", sep = ",")

## write rules in CSV format
rules <- apriori(Epub, parameter = list(support = 0.0005, conf = 0.8))
write(rules, file = "data.csv", sep = ",")

unlink("data.csv") # tidy up
}
\seealso{
Other import/export: 
\code{\link{DATAFRAME}()},
\code{\link{LIST}()},
\code{\link{pmml}},
\code{\link{read}}
}
\author{
Michael Hahsler
}
\concept{import/export}
\keyword{file}
