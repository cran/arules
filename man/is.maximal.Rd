% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.maximal.R
\name{is.maximal}
\alias{is.maximal}
\alias{is.maximal,itemMatrix-method}
\alias{is.maximal,itemsets-method}
\alias{is.maximal,rules-method}
\title{Find Maximal Itemsets}
\usage{
is.maximal(x, ...)

\S4method{is.maximal}{itemMatrix}(x)

\S4method{is.maximal}{itemsets}(x)

\S4method{is.maximal}{rules}(x)
}
\arguments{
\item{x}{the set of \link{itemsets}, \link{rules} or an \link{itemMatrix} object.}

\item{...}{further arguments.}
}
\value{
a logical vector with the same length as \code{x} indicating for
each element in \code{x} if it is a maximal itemset.
}
\description{
Provides the generic function \code{is.maximal()} and methods for
finding maximal itemsets. Maximal frequent itemsets are used as a concise
representation of frequent itemsets. An itemset is maximal in a set if no
proper superset of the itemset is contained in the set (Zaki et al., 1997).
}
\details{
Maximally frequent itemsets can also be mined directly using
\code{\link[=apriori]{apriori()}} or \code{\link[=eclat]{eclat()}} with target "maximally frequent
itemsets".

We define here maximal rules, as the rules generated by maximal itemsets.
}
\references{
Mohammed J. Zaki, Srinivasan Parthasarathy, Mitsunori Ogihara,
and Wei Li (1997). \emph{New algorithms for fast discovery of association
rules}. Technical Report 651, Computer Science Department, University of
Rochester, Rochester, NY 14627.
}
\seealso{
Other postprocessing: 
\code{\link{is.closed}()},
\code{\link{is.generator}()},
\code{\link{is.redundant}()},
\code{\link{is.significant}()},
\code{\link{is.superset}()}

Other associations functions: 
\code{\link{abbreviate}()},
\code{\link{associations-class}},
\code{\link{c}()},
\code{\link{duplicated}()},
\code{\link{extract}},
\code{\link{inspect}()},
\code{\link{is.closed}()},
\code{\link{is.generator}()},
\code{\link{is.redundant}()},
\code{\link{is.significant}()},
\code{\link{is.superset}()},
\code{\link{itemsets-class}},
\code{\link{match}()},
\code{\link{rules-class}},
\code{\link{sample}()},
\code{\link{sets}},
\code{\link{size}()},
\code{\link{sort}()},
\code{\link{unique}()}
}
\author{
Michael Hahsler
}
\concept{associations functions}
\concept{postprocessing}
\keyword{models}
