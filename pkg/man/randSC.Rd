\name{randSC}
\alias{randSC}
\alias{rand.test}
\title{
Randomization tests
}
\description{
This function computes a randomization test for single or multiple baseline single-case datasets. The function is based on an algorithm of the R-package SCRT (Bulte & Onghena, 2009, 2012) but rewritten and extended for the use in AB designs.
}
\usage{
randSC(data, statistic = "Mean B-A", number = 500, complete = FALSE, limit = 5, 
       startpoints = NA, exclude.equal = FALSE, graph = FALSE, output = "c")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}

  \item{statistic}{
Statistic that is computed for randomization. Default is statistic = "Mean B-A").

Arguments for the stat parameter of the rand.test function:

"Mean A-B":Computes the difference between the mean of A and the mean of B. This is used if a level decrease in the B-phase is expected.

"Mean B-A":	Computes the difference between the mean of B and the mean of A. This is used if a level increase in the B-phase is expected.

"Mean |A-B|":	Computes the absolute value between the difference of the mean of the A and B-Phase.

"Median A-B":	The same as Mean A-B but based on the median.

"Median B-A":	The same as Mean B-A but based on the median.

"B":	Computes the difference between the slope parameter of a linear regression model for the B and A-phase. The procedure has not yet been tested for practicality, so it is experimental. 

"Bdecrease":	Complementary to the "B" value. Computes the difference between the slopes of the A and the B-phase. This is useful when a decrease in the slope due to an intervention is expected.

"t plm level":	Computes piecewise regression models and takes the t value for the level predictor as the critical statistic.

"t plm slope":	Computes piecewise regression models and takes the t value for the slope predictor as the critical statistic.

}
  \item{number}{
Size of the randomization distribution. The exactness of the p-value could not exceed 1/number (i.e., number = 100 results in p-values with an exactness of one percent). Default is number = 500. For a faster processing set number = 100. For a more precise p-value set number = 1000.
}
  \item{complete}{
If TRUE, the distribution is based on a complete permutation of all possible starting combinations. This setting overwrites the number Argument. The default setting is FALSE.
}
  \item{limit}{
Minimal length of the A- und B-phase in the randomization sample. Default is limit = 5.
}
  \item{startpoints}{
Alternative to the limit parameter startpoints exactly defines the possible start points of the B-phase. E.g., startpoints = 4:9 restricts possible start points of the B-phase in the randomization design to measurements 4 to 9. startpoints arguments overwrite the limit parameter.
}
  \item{exclude.equal}{
If set FALSE, random distribution value that are equal to the observed distribution are counted as values of the distribution of the null-hypothesis. That is, they decrease the probability of rejecting the null-hypothesis (increase the p-value). exclude.equal should be set to TRUE if you analyse one single-case design (not a multiple baseline data set) to get a sufficient test-power but be aware it increases the chance of an alpha-error.
}
  \item{graph}{
If TRUE, a histogram of the resulting distribution is plotted.
}

  \item{output}{
If set to "C", detailed information are given and if set to "p", only the resulting p value is returned.
}


}

\references{
Bulte, I., & Onghena, P. (2009). Randomization tests for multiple-baseline designs: An extension of the SCRT-R package. Behavior Research Methods, 41(2), 477-485.

Bulte, I., & Onghena, P. (2012). SCRT: Single-Case Randomization Tests. Abgerufen von http://cran.r-project.org/web/packages/SCRT/index.html

}
\author{
Juergen Wilbert
}
\examples{
set.seed(1234)
dat <- rSC(3, d.level = 0.6)
randSC(dat, statistic = "Median B-A", startpoints = 5:17, graph = TRUE)
}

