\name{Mushroom}
\alias{Mushroom}
\docType{data}
\title{Mushroom Data Set}
\description{
  A data set with descriptions of hypothetical samples corresponding to
  23 species of gilled mushrooms in the Agaricus and Lepiota Family,
  classified according to their edibility as (definitely)
  \sQuote{edible} or \sQuote{poisonous} (definitely poisonous, or of
  unknown edibility and not recommended).
}
\usage{
data("Mushroom")
}
\format{
  A data frame with 8124 observations on the following 23 variables.
  \describe{
    \item{\code{classes}}{a factor with levels \code{edible} and
      \code{poisonous}.}
    \item{\code{cap-shape}}{a factor with levels \code{bell},
      \code{conical}, \code{convex}, \code{flat}, \code{knobbed},
      \code{sunken}.}
    \item{\code{cap-surface}}{a factor with levels \code{fibrous},
      \code{grooves}, \code{scaly}, \code{smooth}.}
    \item{\code{cap-color}}{a factor with levels \code{brown},
      \code{buff}, \code{cinnamon}, \code{gray}, \code{green},
      \code{pink}, \code{purple}, \code{red}, \code{white},
      \code{yellow}.}
    \item{\code{bruises?}}{a factor with levels \code{bruises} and
      \code{no}.}
    \item{\code{odor}}{a factor with levels \code{almond}, \code{anise},
      \code{creosote}, \code{fishy}, \code{foul}, \code{musty},
      \code{none}, \code{pungent}, \code{spicy}.}
    \item{\code{gill-attachment}}{a factor with levels \code{attached}
      and \code{free}. }
    \item{\code{gill-spacing}}{a factor with levels \code{close} and
      \code{crowded}.}
    \item{\code{gill-size}}{a factor with levels \code{broad} and
      \code{narrow}.}
    \item{\code{gill-color}}{a factor with levels \code{black},
      \code{brown}, \code{buff}, \code{chocolate}, \code{gray},
      \code{green}, \code{orange}, \code{pink}, \code{purple},
      \code{red}, \code{white}, and \code{yellow}.}
    \item{\code{stalk-shape}}{a factor with levels \code{enlarging} and
      \code{tapering}.}
    \item{\code{stalk-root}}{a factor with levels \code{bulbous},
      \code{club}, \code{equal}, and \code{rooted}. }
    \item{\code{stalk-surface-above-ring}}{a factor with levels
      \code{ibrous}, \code{scaly}, \code{silky}, and \code{smooth}.}
    \item{\code{stalk-surface-below-ring}}{a factor with levels
      \code{ibrous}, \code{scaly}, \code{silky}, and \code{smooth}.}
    \item{\code{stalk-color-above-ring}}{a factor with levels
      \code{brown}, \code{buff}, \code{cinnamon}, \code{gray},
      \code{orange}, \code{pink}, \code{red}, \code{white}, and
      \code{yellow}.}
    \item{\code{stalk-color-below-ring}}{a factor with levels
      \code{brown}, \code{buff}, \code{cinnamon}, \code{gray},
      \code{orange}, \code{pink}, \code{red}, \code{white}, and
      \code{yellow}.}
    \item{\code{veil-type}}{a factor with levels \code{partial}.}
    \item{\code{veil-color}}{a factor with levels \code{brown},
      \code{orange}, \code{white}, and \code{yellow}.}
    \item{\code{ring-number}}{a factor with levels \code{one},
      \code{one}, and \code{two}.} 
    \item{\code{ring-type}}{a factor with levels \code{evanescent},
      \code{flaring}, \code{large}, \code{none}, and \code{pendant}.} 
    \item{\code{spore-print-color}}{a factor with levels \code{black},
      \code{brown}, \code{buff}, \code{chocolate}, \code{green},
      \code{orange}, \code{purple}, \code{white}, and \code{yellow}.}
    \item{\code{population}}{a factor with levels \code{abundant},
      \code{clustered}, \code{numerous}, \code{scattered},
      \code{several}, and \code{solitary}.}
    \item{\code{habitat}}{a factor with levels \code{grasses},
      \code{leaves}, \code{meadows}, \code{paths}, \code{urban},
      \code{waste}, and \code{woods}.}
  }
}
\details{
  The records are drawn from
  G. H. Lincoff (1981) (Pres.),
  \emph{The Audubon Society Field Guide to North American Mushrooms}.
  New York: Alfred A. Knopf.
  (See pages 500--525 for the Agaricus and Lepiota Family.)

  The Guide clearly states that there is no simple rule for determining
  the edibility of a mushroom; no rule like \dQuote{leaflets three, let
    it be} for Poisonous Oak and Ivy.

  Unused levels in the original data were dropped.
}
\source{
  \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
}
\references{
  Blake, C.L. \& Merz, C.J. (1998).
  UCI Repository of Machine Learning Databases.
  Irvine, CA: University of California, Department of Information and
  Computer Science.
}
\examples{
data("Mushroom")
summary(Mushroom)
}
\keyword{datasets}
