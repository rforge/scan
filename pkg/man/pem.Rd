\name{pem}
\alias{pem}
\title{
Percent exceeding the median
}
\description{
The function pem returns the percentage of the B-phase data exceeding the median of the A-phase data. Additionally, a chi square test against a 50/50 distribution is computed. Different measures of central tendency could be addressed for alternative analyses. If you expect a decrease in the B-phase, set the parameter decreasing = TRUE.
}
\usage{
pem(data, decreasing = FALSE, binom.test = TRUE, chi.test = FALSE, FUN = median, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}
  \item{decreasing}{
If you expect the data to be lower in the B phase, set decreasing = TRUE (default is FALSE).
}
  \item{binom.test}{
Computes a binomial test for a 50/50 distribution. By default set TRUE.
}
  \item{chi.test}{
By default set FALSE. If set FALSE the Chi-square Test is skipped.
}
  \item{FUN}{
By default median. Alternatively, you could implement any other function for computing an average.
}
  \item{\dots}{
Additional arguments for the FUN parameter (e.g. FUN = mean, trim = 0.1 will use a 10 percent trimmed mean instead of a median for analysing the data).
}
}
\author{
Juergen Wilbert
}
\examples{
set.seed(1234)
dat <- rSC(5, d.level = 0.5)
pem(dat, chi.test = TRUE)
}

