\name{outlierSC}
\alias{outlierSC}
\title{Cleaning outliers}
\description{
Identifies and drops outliers for each case of your data file. The function returns a new data file without the outlier data. Criteria for outlier identification can be standard deviations, confidence intervals, and Cook's distance (based on a piecewise linear regression model).
}
\usage{
outlierSC(data, criteria = c("SD", "2"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}
  \item{criteria}{
Specifies criteria for dropping data.
SD for standard deviations (e.g., criteria = c("SD", 2)).
CI for Confidence interval (e.g., criteria = c("CI", 0.99) represents a 99 percent confidence interval.
Cook for Cook's distance based on the piecewise-linear-regression model. To exclude cases where cook exceeds 4/n set: criteria = c("Cook", "4/n").
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{data}{Cleaned up data file.}
\item{dropped.n}{A list with the number of dropped values for each case.}
\item{dropped.mt}{A list with the measuremt-times of dropped cases for each case (values are based on the mt variable of each data frame).}
\item{ci.matrix}{A list with a matrix for each case with values for the upper and lower boundaries based on the confidence interval.}
\item{sd.matrix}{A list with a matrix for each case with values for the upper and lower boundaries based on the standard deviation.}
\item{cook}{A list with the cook values for each measuremnt-time of each case.}
\item{criteria}{Method and criteria used for outlier analysis.}
\item{N}{Number of cases in the design.}
\item{case.names}{Identifier of the cases.}

}

\author{
Juergen Wilbert
}
\examples{
#To 'clean' the original data from Grosche (2011) without the outliers and using 
#Cook's distance greater 4/n as a criteria, type:
(res <- outlierSC(Grosche2011, criteria = c("Cook", "4/n")))
plotSC(Grosche2011, marks = list(positions = res$dropped.mt))


}
