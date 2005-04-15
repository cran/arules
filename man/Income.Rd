\name{Income}
\alias{Income}
\alias{Income_orig}
\docType{data}
\title{Income Data Set}
\description{
  The \code{Income} data set originates from an example in the book
  \sQuote{The Elements of Statistical Learning} (see Section source). 
  The dataset is an extract from this survey.  It consists of 8993
  instances (obtained from the original dataset with 9409 instances, by
  removing those observations with the annual income missing) with 14
  demographic attributes.  The data set is a good mixture of categorical
  and continuous variables with a lot of missing data.  This is
  characteristic of data mining applications.
}
\usage{
data("Income")
data("Income_orig")
}
\format{
  A data frame with 8993 observations on the following 14 variables.
  \describe{
    \item{income}{a factor with levels \code{$0-$40,000},
      \code{$40,000+}} 
    \item{sex}{a factor with levels \code{male}, \code{female}} 
    \item{marital status}{a factor with levels \code{married},
      \code{cohabitation}, \code{divorced}, \code{widowed},
      \code{single}}
    \item{age}{a factor with levels \code{14-34}, \code{35+}}
    \item{education}{a factor with levels \code{no college graduate},
      \code{college graduate}}
    \item{occupation}{a factor with levels
      \code{professional/managerial}, \code{sales}, \code{laborer},
      \code{clerical/service}, \code{homemaker}, \code{student},
      \code{military}, \code{retired}, \code{unemployed}}
    \item{years in bay area}{a factor with levels \code{1-9},
      \code{10+}} 
    \item{dual incomes}{a factor with levels \code{not married},
      \code{yes}, \code{no}}
    \item{number in household}{a factor with levels \code{1}, \code{2+}} 
    \item{number of children}{a factor with levels \code{0}, \code{1+}}
    \item{householder status}{a factor with levels \code{own},
      \code{rent}, \code{live with parents/family}}
    \item{type of home}{a factor with levels \code{house},
      \code{condominium}, \code{apartment}, \code{mobile Home},
      \code{other}}
    \item{ethnic classification}{a factor with levels \code{american
	indian}, \code{asian}, \code{black}, \code{east indian},
      \code{hispanic}, \code{pacific islander}, \code{white},
      \code{other}} 
    \item{language in home}{a factor with levels \code{english},
      \code{spanish}, \code{other}}
  }
}
\details{
  The original data frame is available as data set
  \code{Income_original}.  For the \code{Income} data set we
  preprocessed the data as described in \sQuote{The Elements of
    Statistical Learning} by cutting each ordinal variable (age,
  education, income, years in bay area, number in houshold, and number
  of children) at its median into two values. 
}
\source{
  Impact Resources, Inc., Columbus, OH (1987).

  Obtained from the web site of the book:
  Hastie, T., Tibshirani, R. \& Friedman, J. (2001).
  \emph{The Elements of Statistical Learning}.
  Springer-Verlag.
  (\url{http://www-stat.stanford.edu/~tibs/ElemStatLearn/};
  called \sQuote{Marketing})
}
\examples{
data("Income")

Income_transactions <- as(Income, "transactions")
summary(Income_transactions)
}
\keyword{datasets}
