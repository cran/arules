\name{categorize}
\alias{categorize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a Continuous Variable into a Categorical Variable
}
\description{
This function converts a continuous variable into a categorical variable (factor) suitable for association rule mining.
}
\usage{
categorize(x, breaks = 4, quantile = TRUE, labels = NULL, ...)
}
\arguments{
  \item{x}{a numeric vector (continuous variable).
}
  \item{breaks}{ number of categories or breaks for categories (quantiles or absolute values).
}
  \item{quantile}{
logical; use quantile (or absolute values) to determine the category boundaries. 
}
  \item{labels}{
character vector; names for categories.
}

  \item{\dots}{
further arguments passed on to \code{cut()}.
}
}
%\details{
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A factor representing the categorized continuous variable.

}
%\references{
%}
\seealso{
\code{\link{cut}} in \pkg{base} and
\code{\link{quantile}} in \pkg{stats}.
}
\examples{
data(iris)
head(iris)

### convert continuous variables into categories
### default (4 categories - 0-25%, 25-50%, 50-75% and 75-100% quantiles)
iris[,1] <- categorize(iris[,1])
### specify quantiles
iris[,2] <- categorize(iris[,2], breaks=c(0,.25,.75,1))
### specify absolute boundaries
iris[,3] <- categorize(iris[,3], breaks=c(0,1,2,3,4,5,6,7), quantile=FALSE)
### name categories
iris[,4] <- categorize(iris[,4], breaks=3, quantile=FALSE, 
	labels=c("short", "medium", "long"))

head(iris)

### convert dataset into transactions
tr <- as(iris, "transactions")
tr

### mine and inspect rules
rules<-apriori(tr)
inspect(head(sort(rules, by="lift")))


}
\keyword{manip}
