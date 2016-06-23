\name{nap}
\alias{nap}
\title{Non-overlap of all Pairs}
\description{
The index Nonoverlapping of all pairs was developed by Parker and colleagues (Parker & Vannest, 2009). It is recommended as an overlapping measurement.
}
\usage{
nap(data, decreasing = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame or a list of data frames.
}
  \item{decreasing}{
If you expect the data to be lower in the B phase, set decreasing = TRUE (default is FALSE).
}
}
\value{
  \item{NAP}{Nonoverlap of all pairs.}
  \item{N}{Number of cases.}
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

