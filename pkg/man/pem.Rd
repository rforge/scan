\name{pem}
\alias{pem}
\title{Percent exceeding the median}
\description{
This function returns the Percentage of the B-phase data exceeding the median of the A-phase data. Additionally, a chi square test against a 50/50 distribution is computed. Different measures of central tendency could be addressed for alternative analyses. If you expect a decrease in the B-phase, set the parameter decreasing = TRUE.
}
\usage{
pem(data, decreasing = FALSE, binom.test = TRUE, chi.test = FALSE, FUN = median, ...)
}
\arguments{
  \item{data}{
A single-case data frame or a list of single-case data frames. See \code{\link{makeSCDF}} to learn about this format.
}
  \item{decreasing}{
If you expect data to be lower in the B phase, set \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
}
  \item{binom.test}{
Computes a binomial test for a 50/50 distribution. Default is \code{binom.test = TRUE}.
}
  \item{chi.test}{
Computes a Chi-square test. The default setting \code{chi.test = FALSE} skips the Chi-square test.
}
  \item{FUN}{
Data points are compared with the phase A median. Use this argument to implement alternative measures of central tendency. Default is \code{FUN = median}
}
  \item{\dots}{
Additional arguments for the \code{FUN} parameter (e.g. \code{FUN = mean, trim = 0.1} will use the 10 percent trimmed arithmetic mean instead of the median for comparisons).
}
}
\value{
  \item{PEM}{Percent exceeding the median.}
  \item{test}{A list of \code{test} results.}
  \item{decreasing}{Logical argument from function call (see \code{Arguments} above).}
}
\author{
Juergen Wilbert
}
\examples{
## Calculate the PEM including the Binomial and Chi-square tests
set.seed(1234)
dat <- rSC(5, d.level = 0.5)
pem(dat, chi.test = TRUE)
}