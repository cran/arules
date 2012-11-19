\name{interestMeasure}
\alias{interestMeasure}
\alias{interestMeasure,rules-method}
\alias{interestMeasure,itemsets-method}
\title{Calculating various additional interest measures}
\description{
Provides the generic function \code{interestMeasure} and the needed S4 method 
to calculate various additional interest measures for existing sets of
itemsets or rules.
}
\usage{
interestMeasure(x, method, transactions = NULL, reuse = TRUE, ...)
}
\arguments{
  \item{x}{ a set of itemsets or rules. }
  \item{method}{ name or vector of names of the desired interest measures 
      (see details for available measures).} 
  \item{transactions}{ the transaction data set used to mine 
    the associations. }
  \item{reuse}{ logical indicating if information in quality slot should
      be reuse for calculating the measures. This speeds up the process
      significantly since only very little (or no) transaction counting 
      is necessary if support, confidence and lift are already available.
      Use \code{reuse=FALSE} to force counting (might be very slow).}
  \item{\dots}{ further arguments for the measure calculation. }
}
\details{
For itemsets the following measures are implemented:  
\describe{
\item{"allConfidence"}{ (see, Omiencinski, 2003) is defined on itemsets as the
minimum confidence of all possible rule generated from the itemset.}
\item{"crossSupportRatio"}{ (see, Xiong et al., 2003) is defined on itemsets as
the ratio of the support of the least frequent item to the support of the most
frequent item.  Cross-support patterns have a ratio smaller than a set
threshold. Normally many found patterns are cross-support patterns which
contain frequent as well as rare items. Such patterns often tend to be
spurious.}
\item{"lift"}{ probability (support) of the itemset over the product of the 
probabilities of all items in the itemset. This is a measure of 
dependence similar to lift for rules.}
\item{"support"}{calculate itemset support.}
}


For rules the following measures are implemented:  
\describe{
\item{"chiSquare"}{ (see Liu et al. 1999). The chi-square statistic 
  to test for independence between the lhs and rhs of the rule.
  The critical value of the chi-square distribution with \eqn{1} degree of 
  freedom (2x2 contingency table) at \eqn{\alpha=0.05}{alpha=0.05} 
  is \eqn{3.84}; higher chi-square
  values indicate that the lhs and the rhs are not independent.  
  %Note that if
  %any cell in the contingency table has an expected value below \eqn{5}, the
  %chi-square value is set to \code{NA} 
  %since the approximation used in the chi-square test breaks down.
  Note that the contingency table is likely to have cells with
  low expected values and that thus
  Fisher's Exact Test might be more appropriate 
  (see below).
  }
\item{"confidence"}{calculate rule confidence. Range \eqn{0\ldots1}.}
\item{"conviction"}{ (see Brin et al. 1997) defined as 
  \eqn{P(X)P(\overline{Y})/P(X \wedge \overline{Y})}{P(X)P(not Y)/P(X and not Y)}. 
  Range: \eqn{0.5\ldots1\ldots\infty}{0.5\ldots1\ldots Inf} (\eqn{1} indicates unrelated items).}
\item{"cosine"}{(see Tan et al. 2004) equivalent to the IS measure. 
  Range: \eqn{0\ldots1}. 
  }
\item{"coverage"}{calculate rule coverage (support of LHS). 
    Range: \eqn{0\ldots1}.}
\item{"doc"}{calculate difference of confidence, which is defined
by Hofmann and Wilhelm (2001) as 
\eqn{\mathrm{conf}(X \Rightarrow Y) - \mathrm{conf}(\overline X \Rightarrow Y)}{conf(X -> Y)-conf(!X -> Y)}.
    Range: \eqn{-1\ldots1}.}
\item{"gini"}{gini index (see Tan et al. 2004). Range: \eqn{0\ldots1}.}
\item{"hyperLift"}{ (see, Hahsler and Hornik, 2007) is an adaptation of the lift
measure which is more robust for low counts. It is based on the idea that under
independence the count \eqn{c_{XY}} of the transactions which contain all items
in a rule \eqn{X \Rightarrow Y}{X -> Y} follows a hypergeometric distribution 
(represented by the random variable \eqn{C_{XY}}) with
the parameters given by the counts  \eqn{c_X} and  \eqn{c_Y}.
  
Lift is defined for the rule \eqn{X \Rightarrow Y}{X -> Y} as:
  \deqn{\mathrm{lift}(X \Rightarrow Y) = \frac{P(X \cup Y)}{P(X)P(Y)} 
  = \frac{c_{XY}}{E[C_{XY}]},}{lift(X -> Y) = P(X+Y)/(P(X)*P(Y)) = c_XY / E[C_XY],}
  
where \eqn{E[C_{XY}] = c_X c_Y / m} with \eqn{m} being the number
of transactions in the database.

Hyper-lift is defined as:
\deqn{\mathrm{hyperlift}(X \Rightarrow Y) = \frac{c_{XY}}{Q_{\delta}[C_{XY}]},}{hyperlift(X -> Y) = c_XY / Q_d[C_XY],}

where  \eqn{Q_{\delta}[C_{XY}]}{Q_d[C_XY]} is the
quantile of the hypergeometric distribution given by \eqn{\delta}{d}.
The quantile can be given
as parameter \code{d} (default: \code{d=0.99}).
Range: \eqn{0\ldots\infty}{0\ldots Inf}.
}
  
\item{"hyperConfidence"}{ (Hahsler and Hornik, 2007)
  calculates the confidence level that we observe too high/low counts 
  for rules \eqn{X \Rightarrow Y}{X -> Y} using the hypergeometric model.
  Since the counts are drawn from a hypergeometric distribution 
  (represented by the random variable \eqn{C_{XY}}) with
  known parameters given by the counts  \eqn{c_X} and  \eqn{c_Y},
  we can calculate a confidence interval for the observed counts 
  \eqn{c_{XY}} stemming from the distribution. Hyper-confidence
  reports the confidence level 
  (significance level if \code{significance=TRUE} is used) for
  \describe{
  \item{complements -}{
    \eqn{1 - P[C_{XY} >= c_{XY} | c_X, c_Y]}
  }
  \item{substitutes -}{
    \eqn{1 - P[C_{XY} < c_{XY} | c_X, c_Y]}.
  }
  }
  A confidence level of, e.g., \eqn{> 0.95} indicates that
  there is only a  5\% chance that the count for the rule was generated
  randomly.
  
  Per default complementary effects are mined, substitutes can be found
  by using the parameter \code{complements = FALSE}. 
  Range: \eqn{0\ldots1}.
  }
\item{"fishersExactTest"}{Fisher's Exact Test is a statistical significance test used in the analysis of contingency tables where sample sizes are small. 
 Returns the p-value.
 Note that it is equal to hyper-confidence with \code{significance=TRUE}.}
\item{"improvement"}{(see Bayardo et al. 2000)
  the  improvement of a rule is 
  the minimum difference between its confidence and the confidence of any
  proper sub-rule with the same consequent. Range: \eqn{0\ldots1}.}
\item{"leverage"}{(see Piatetsky-Shapiro 1991)
  defined as \eqn{P(X \Rightarrow Y) - (P(X)P(Y))}{P(X->Y) - (P(X)P(Y))}.
  It measures the difference of X and Y appearing together in the data set 
  and what would be expected if X and Y where statistically dependent. 
  Range: {-1\ldots1}.}
\item{"lift"}{calculate rule lift. Range: \eqn{0\ldots\infty}{0\ldots Inf}.}
\item{"oddsRatio"}{(see Tan et al. 2004).
  The odds of finding X in transactions which contain Y divided by
  the odds of finding X in transactions which do not contain Y.
  Range: \eqn{0\ldots1\ldots\infty}{0\ldots1\ldots Inf} (
  \eqn{1} indicates that Y is not associated to X).} 
\item{"phi"}{ the correlation coefficient \eqn{\phi}{phi} 
  (see Tan et al. 2004) Range: \eqn{-1} (perfect neg. correlation)
  to \eqn{+1} (perfect pos. correlation).}
\item{"RLD"}{(Relative Linkage Disequilibrium; see Kenett and Salini 2008).
    RLD evaluates the deviation
        of the support of the whole rule from the support expected under in-
        dependence given the supports of the LHS and the RHS. The code was
        contributed by Silvia Salini. Range: \eqn{0\ldots1}.}
\item{"support"}{calculate rule support. Range: \eqn{0\ldots1}.}
}
}
\value{
   If only one method is used, the function returns a numeric vector 
   containing the values of the interest measure for each association
   in the set of associations \code{x}. 

   If more than one methods are specified, the result is a data.frame 
   containing the different measures for each association.
}
\references{
R. Bayardo, R. Agrawal, and D. Gunopulos (2000). Constraint-based rule mining
in large, dense databases.  \emph{Data Mining and Knowledge Discovery},
4(2/3):217--240, 2000.

Sergey Brin, Rajeev Motwani, Jeffrey D. Ullman, and Shalom Tsur (1997). Dynamic
itemset counting and implication rules for market basket data. In \emph{SIGMOD
1997, Proceedings ACM SIGMOD International Conference on Management of Data},
pages 255--264, Tucson, Arizona, USA.

Michael Hahsler and Kurt Hornik. New probabilistic interest measures for association rules. \emph{Intelligent Data Analysis}, 11(5):437--455, 2007

Heike Hofmann and Adalbert Wilhelm. Visual comparison of association rules. 
\emph{Computational Statistics}, 16(3):399--415, 2001.

Ron Kenett and Silvia Salini. Relative Linkage Disequilibrium: A New
measure for association rules. In \emph{8th Industrial Conference on 
Data Mining ICDM 2008 July 16--18, 2008, Leipzig/Germany,} to appear, 2008.

Bing Liu, Wynne Hsu, and Yiming Ma (1999). Pruning and summarizing the
discovered associations. In \emph{KDD '99: Proceedings of the fifth ACM SIGKDD
international conference on Knowledge discovery and data mining}, pages
125--134.  ACM Press, 1999.

Edward R. Omiecinski (2003). Alternative interest measures for mining
associations in databases. \emph{IEEE Transactions on Knowledge and Data
Engineering}, 15(1):57--69, Jan/Feb 2003.

Pang-Ning Tan, Vipin Kumar, and Jaideep Srivastava (2004). Selecting the right
objective measure for association analysis. \emph{Information Systems},
29(4):293--313.

Piatetsky-Shapiro, G. (1991). Discovery, analysis, and presentation of strong
rules. In: \emph{Knowledge Discovery in Databases}, pages 229--248.

Hui Xiong, Pang-Ning Tan, and Vipin Kumar (2003). Mining strong affinity
association patterns in data sets with skewed support distribution. In Bart
Goethals and Mohammed J. Zaki, editors, \emph{Proceedings of the IEEE
International Conference on Data Mining}, November 19--22, 2003, Melbourne,
Florida, pages 387--394.
}

\seealso{ \code{\link{itemsets-class}}, \code{\link{rules-class}} }
\examples{
data("Income")
rules <- apriori(Income)

## calculate a single measure and add it to the quality slot
quality(rules) <- cbind(quality(rules), 
	hyperConfidence = interestMeasure(rules, method = "hyperConfidence", 
	Income))

inspect(head(sort(rules, by = "hyperConfidence")))

## calculate several measures
m <- interestMeasure(rules, c("confidence", "oddsRatio", "leverage"), Income)
inspect(head(rules))
head(m)
}
\keyword{models}
