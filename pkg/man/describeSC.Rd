\name{describeSC}
\alias{describeSC}
\title{
Descriptives
}
\description{
The describeSC command gives some important descriptives of your single-case data including trends, standardized mean differences, measurements for data overlap, and autocorrelation.
}
\usage{
describeSC(data, decreasing = FALSE)
}

\arguments{
  \item{data}{
A data frame or a list of data frames.
}
  \item{decreasing}{
If you expect the data to be lower in the B-phase, set decreasing = TRUE.
}
}

\author{
Juergen Wilbert
}

\examples{
set.seed(1234)
dat <- rSC(5, B.start = round(runif(5,5,10)), d.level = 1.0, d.slope = 0.1, d.trend = 0.05, 
rtt = 0.75, round = 0, random.names = TRUE)
describeSC(dat)

set.seed(1233)
dat <- rSC(5, B.start = round(runif(5,5,10)), d.level = -0.4, d.slope = 0.0, d.trend = 0.0, 
rtt = 0.70, random.names = TRUE)
describeSC(dat, decreasing = TRUE)

}

