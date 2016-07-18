\name{nap}
\alias{nap}
\title{Nonoverlap of all Pairs}
\description{
The index Nonoverlap of all pairs was developed by Parker and Vannest (2009). It is recommended as overlapping measurement.
}
\usage{
nap(data, decreasing = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A single-case data frame.
}
  \item{decreasing}{
If you expect the data to be lower in the B phase, set decreasing = TRUE (default is FALSE).
}
}
\value{
  \item{N}{Number of cases.}
  \item{NAP}{Nonoverlap of all pairs.}
  \item{Rescaled NAP}{NAP rescaled to 0-100\%.}
}
\references{
Parker, R. I., & Vannest, K. (2009). An improved effect size for single-case research: Nonoverlap of all pairs. Behavior Therapy, 40(4), 357-367.
}
\author{
Juergen Wilbert
}

\examples{
set.seed(1234)
dat <- rSC(1, d.level = -1.0)
nap(dat, decreasing = TRUE)
}

