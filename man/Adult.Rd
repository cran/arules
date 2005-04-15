\name{Adult}
\alias{Adult}
\alias{Adult_transactions}
\docType{data}
\title{Adult Data Set}
\description{
  The \code{Adult} data set contains the questionnaire data of the
  \dQuote{Adult} database (originally called the \dQuote{Census Income}
  Database) formatted as a \code{data.frame} prepared for use with
  \pkg{arules}.  The \code{Adult_transactions} data set contains the data 
  already coerced to \code{\link[transactions-class]{transactions}}.
}
\usage{
data("Adult")
data("Adult_transactions")
}
\details{
  The \dQuote{Adult} database was extracted from the census bureau
  database found at
  \url{http://www.census.gov/ftp/pub/DES/www/welcome.html} in 1994 by
  Ronny Kohavi and Barry Becker, Data Mining and Visualization, Silicon
  Graphics. It was originally used to predict whether income exceeds
  \$50K/yr based on census data.
 
  To prepare the data set for association mining, we removed the
  continuous attribute \code{fnlwgt} (final weight) and added the
  attribute \code{salary} with levels \sQuote{small} and \sQuote{large}
  ($>$\$50K/yr).  The original data contained 5 more continuous
  attributes (\code{age}, \code{education-num}, \code{capital-gain},
  \code{capital-loss} and \code{hours-per-week}) which we coded using
  discrete values.
}
\format{
  \code{Adult} contains a \code{data.frame} with 48842 observations on
  the following 14 variables:
  \describe{
    \item{age}{a factor with levels \code{middle-aged}, \code{old},
      \code{senior}, \code{young}}
    \item{workclass}{a factor with levels \code{Federal-gov},
      \code{Local-gov}, \code{Never-worked}, \code{Private},
      \code{Self-emp-inc}, \code{Self-emp-not-inc}, \code{State-gov},
      \code{Without-pay}}
    \item{education}{a factor with levels \code{10th}, \code{11th},
      \code{12th}, \code{1st-4th}, \code{5th-6th}, \code{7th-8th},
      \code{9th}, \code{Assoc-acdm}, \code{Assoc-voc}, \code{Bachelors},
      \code{Doctorate}, \code{HS-grad}, \code{Masters},
      \code{Preschool}, \code{Prof-school}, \code{Some-college}}
    \item{education-num}{a factor with levels \code{1}, \code{10},
      \code{11}, \code{12}, \code{13}, \code{14}, \code{15}, \code{16},
      \code{2}, \code{3}, \code{4}, \code{5}, \code{6}, \code{7},
      \code{8}, \code{9}} 
    \item{marital-status}{a factor with levels \code{Divorced},
      \code{Married-AF-spouse}, \code{Married-civ-spouse},
      \code{Married-spouse-absent}, \code{Never-married},
      \code{Separated}, \code{Widowed}} 
    \item{occupation}{a factor with levels \code{Adm-clerical},
      \code{Armed-Forces}, \code{Craft-repair}, \code{Exec-managerial},
      \code{Farming-fishing}, \code{Handlers-cleaners},
      \code{Machine-op-inspct}, \code{Other-service},
      \code{Priv-house-serv}, \code{Prof-specialty},
      \code{Protective-serv}, \code{Sales}, \code{Tech-support},
      \code{Transport-moving}} 
    \item{relationship}{a factor with levels \code{Husband},
      \code{Not-in-family}, \code{Other-relative}, \code{Own-child},
      \code{Unmarried}, \code{Wife}} 
    \item{race}{a factor with levels \code{Amer-Indian-Eskimo},
      \code{Asian-Pac-Islander}, \code{Black}, \code{Other},
      \code{White}} 
    \item{sex}{a factor with levels \code{Female}, \code{Male}}
    \item{capital-gain}{a factor with levels \code{high}, \code{medium},
      \code{none}, \code{small}}
    \item{capital-loss}{a factor with levels \code{medium}, \code{none},
      \code{small}}
    \item{hours-per-week}{a factor with levels \code{full-time},
      \code{half-time}, \code{overtime}, \code{too-many}}
    \item{native-country}{a factor with levels \code{Cambodia},
      \code{Canada}, \code{China}, \code{Columbia}, \code{Cuba},
      \code{Dominican-Republic}, \code{Ecuador}, \code{El-Salvador},
      \code{England}, \code{France}, \code{Germany}, \code{Greece},
      \code{Guatemala}, \code{Haiti}, \code{Holand-Netherlands},
      \code{Honduras}, \code{Hong}, \code{Hungary}, \code{India},
      \code{Iran}, \code{Ireland}, \code{Italy}, \code{Jamaica},
      \code{Japan}, \code{Laos}, \code{Mexico}, \code{Nicaragua},
      \code{Outlying-US(Guam-USVI-etc)}, \code{Peru},
      \code{Philippines}, \code{Poland}, \code{Portugal},
      \code{Puerto-Rico}, \code{Scotland}, \code{South}, \code{Taiwan},
      \code{Thailand}, \code{Trinadad&Tobago}, \code{United-States},
      \code{Vietnam}, \code{Yugoslavia}}
    \item{salary}{a factor with levels \code{small}, \code{large}}
  }
}
\source{
  \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
}
\references{
  Blake, C.L. \& Merz, C.J. (1998).
  UCI Repository of Machine Learning Databases.
  Irvine, CA: University of California, Department of Information and
  Computer Science.

  The data set was first cited in 
  Kohavi, R. (1996).
  Scaling Up the Accuracy of Naive-Bayes Classifiers: a Decision-Tree
  Hybrid.
  \emph{Proceedings of the Second International Conference on Knowledge
    Discovery and Data Mining}. 
}
\examples{
data("Adult")
dim(Adult)
Adult[1:2, 1:4]

data("Adult_transactions")
Adult_transactions
}
\keyword{datasets}
