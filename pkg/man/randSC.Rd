\name{randSC}
\alias{randSC}
\alias{rand.test}
\title{Randomization Tests for single-case data}
\description{
The randSC function computes a randomization test for single or multiple baseline single-case datasets. The function is based on an algorithm from the \code{\link{SCRT}} package SCRT (Bulte & Onghena, 2009, 2012), but rewritten and extended for the use in AB designs.
}

\usage{
randSC(data, statistic = "Mean B-A", number = 500, complete = FALSE, limit = 5, 
       startpoints = NA, exclude.equal = FALSE, graph = FALSE, output = "c")
}

\arguments{
  \item{data}{A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.}
  \item{statistic}{Defines the statistic on which the comparison of phases A and B is based on. Default setting is \code{statistic = "Mean B-A"}). The following comparisons are possible:
    \itemize{
      \item{\code{"Mean A-B"}: Uses the difference between the mean of phase A and the mean of phase B. This is appropriate if a decrease of scores was expected for phase B.}
      \item{\code{"Mean B-A"}:	Uses the difference between the mean of phase B and the mean of phase A. This is appropriate if an increase of scores was expected for phase B.}
      \item{\code{"Mean |A-B|"}: Uses the absolute value of the difference between the means of phases A and B.}
      \item{\code{"Median A-B"}: The same as \code{"Mean A-B"}, but based on the median.}
      \item{\code{"Median B-A"}: The same as \code{"Mean B-A"}, but based on the median.
      \item{\code{"B"}: Caution! This procedure is still under development. Do not use it! Uses the difference between the slope parameter of a linear regression model for phases B and A.}
      \item{\code{"Bdecrease"}: Caution! This procedure is still under development. The same as \code{"B"}, but A-B. This is used is a .}
      \item{}
    }
"t plm level": Computes piecewise regression models and takes the t value for the level predictor as the critical statistic.

"t plm slope": Computes piecewise regression models and takes the t value for the slope predictor as the critical statistic.
}
  \item{number}{Size of the randomization distribution. The exactness of the p-value can not exceed 1/number (i.e., number = 100 results in a p-values with an exactness of one percent). Default is number = 500. For a faster processing set number = 100. For a more precise p-value set number = 1000.}
  \item{complete}{
If TRUE, the distribution is based on a complete permutation of all possible starting combinations. This setting overwrites the number Argument. The default setting is FALSE.
}
  \item{limit}{
Minimal length of the A- und B-phase in the randomization sample. Default is limit = 5.}
  \item{startpoints}{Alternative to the limit parameter startpoints exactly defines the possible start points of the B-phase. E.g., startpoints = 4:9 restricts possible start points of the B-phase in the randomization design to measurements 4 to 9. startpoints arguments overwrite the limit parameter.}
  \item{exclude.equal}{If set FALSE, random distribution value that are equal to the observed distribution are counted as values of the distribution of the null-hypothesis. That is, they decrease the probability of rejecting the null-hypothesis (increase the p-value). exclude.equal should be set to TRUE if you analyse one single-case design (not a multiple baseline data set) to get a sufficient test-power but be aware it increases the chance of an alpha-error.}
  \item{graph}{If TRUE, a histogram of the resulting distribution is plotted.}
  \item{output}{If set to "C", detailed information are given and if set to "p", only the resulting p value is returned.}
}

\value{
  \item{statistic}{}
  \item{N}{}
  \item{n1}{}
  \item{n2}{}
  \item{limit}{}
  \item{startpoints}{}
  \item{p.value}{}
  \item{number}{}
  \item{complete}{}
  \item{observed.statistic}{}
  \item{Z}{}
  \item{p.z.single}{}
  \item{distribution}{}
  \item{possible.combinations}{}
  \item{auto.corrected.number}{}
}

\references{
Bulte, I., & Onghena, P. (2009). Randomization tests for multiple-baseline designs: An extension of the SCRT-R package. \emph{Behavior Research Methods, 41}, 477-485.

Bulte, I., & Onghena, P. (2012). \emph{SCRT: Single-Case Randomization Tests}. Available from: http://cran.r-project.org/web/packages/SCRT/index.html
}

\author{
Juergen Wilbert
}

\examples{
set.seed(1234)
dat <- rSC(3, d.level = 0.6)
randSC(dat, statistic = "Median B-A", startpoints = 5:17, graph = TRUE)
}

