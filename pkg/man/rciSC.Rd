\name{rciSC}
\alias{rciSC}
\alias{rCi}
\title{Reliable change index}
\description{
CAUTION! This function is still under development and not ready for use! This function computes three indices of reliable change (Wise, 2004) and corresponding descriptive statistics.
}
\usage{
rciSC(data, rel = 0.8, ci = 0.95, graph = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A single-case data frame. See \code{\link{makeSCDF}} to learn about this format.
}
  \item{rel}{
Reliability of the measure, used to compute the standard error. Default is \code{rel = 0.8}.
}
  \item{ci}{
Width of confidence interval as a decimal. Default is \code{ci = 0.95} applying a 95\%-confidence interval.
}
  \item{graph}{
If set \code{TRUE}, a box plot of phase A and B scores is displayed. \code{graph = FALSE} by default.
}
}
\references{
Wise, E. A. (2004). Methods for analyzing psychotherapy outcomes: A review of clinical significance, reliable change, and recommendations for future directions. \emph{Journal of Personality Assessment, 82}, 50 - 59.
}
\value{
  \item{RCI}{A list of three RCI calculations (Jacobson et al., Christenden et al., Hageman et al.).}
  \item{stand.dif}{Standardized difference between mean phase A and B scores.}
  \item{conf}{A matrix containing the lower and upper confidence interval boundaries of phases A and B.}
  \item{conf.percent}{Numeric argument from function call (see \code{ci} in \code{Arguments} section).}
  \item{reliability}{Numeric argument from function call (see \code{Arguments} above).}
  \item{descriptives}{A matrix containing descriptive statistics for phases A and B: n, mean, SD, SE.}
  \item{N}{Number of cases.}
  \item{A}{A vector of phase A scores.}
  \item{B}{A vector of phase B scores.}
}
\author{
Juergen Wilbert
}
\examples{
## report rci of the first case from the byHeart data and include a graph
rciSC(byHeart2011[1], graph = TRUE)
}