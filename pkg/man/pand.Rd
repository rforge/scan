\name{pand}
\alias{pand}

\title{
Percentage of all non-overlapping data
}
\description{
The percentage of all non-overlapping data (PAND; Parker, Hagan-Burke, & Vannest, 2007) is an index for quantifying a level increase (or decrease) in performance after the onset of an intervention. PAND indicates nonoverlap between phase A and B data (like \code{PND}), but uses all data and is therefore not based on one single (probably unrepresentative) datapoint. Furthermore, PAND allows the comparison of real and expected associations (Chi-square test) and estimation of the effect size Phi, which equals Pearsons r for dichotomous data. Thus Phi-Square is the amount of explained variance. The original procedure for computing the PAND (Parker, Hagan-Burke, & Vannest, 2007) does not account for ambivalent datapoints (ties). The newer \code{NAP} overcomes this problem and has better precision-power (Parker, Vannest, & Davis, 2014).
}
\usage{
pand(data, decreasing = FALSE, correction = TRUE)
}
 
\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.
}

  \item{decreasing}{
If you expect data to be lower in the B phase, set \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
}
  \item{correction}{
The default \code{correction = TRUE} makes \code{pand} use a frequency matrix, which is corrected for ties. A tie is counted as the half of a measurement in both phases. Set \code{correction = FALSE} do use the uncorrected matrix, which is not recommended.
}
}

\references{
Parker, R. I., Hagan-Burke, S., & Vannest, K. (2007). Percentage of All Non-Overlapping Data (PAND): An Alternative to PND. The Journal of Special Education, 40, 194-204.
Parker, R. I., & Vannest, K. (2009). An Improved Effect Size for Single-Case Research: Nonoverlap of All Pairs. Behavior Therapy, 40, 357-367.
}
\author{
Juergen Wilbert
}


\examples{
## PAND for a MBD
gunnar <- makeSCDF(c(2,3,1,5,3,4,2,6,4,7), B.start = 5)
birgit <- makeSCDF(c(3,3,2,4,7,4,2,1,4,7), B.start = 4)
bodo <- makeSCDF(c(2,3,4,5,3,4,7,6,8,7), B.start = 6)
mbd <- list(gunnar, birgit, bodo)
pand(mbd)
pand(bodo)

## PAND with expecting a decrease of scores in phase B
cubs <- makeSCDF(c(20,22,24,17,21,13,10,9,20,9,18), B.start = 5)
pand(cubs, decreasing = TRUE)
}

